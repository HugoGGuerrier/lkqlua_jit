//! # Frame module
//!
//! This module contains all required entities to represents frames during the
//! compilation process.

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    ops::Range,
    rc::Rc,
};

use crate::{bytecode::extended_bytecode::Label, sources::SourceSection};

/// This type represents the concept of framing in the intermediate tree. This
/// is where local variables and up-values lives.
pub struct Frame {
    /// A reference to the frame that owns this one.
    pub parent_frame: Option<Rc<RefCell<Frame>>>,

    /// Slots of the frame, each one being associated with its name.
    pub bindings: HashMap<String, BindingData>,

    /// Variant part of the frame, containing additional information.
    pub variant: FrameVariant,
}

pub enum FrameVariant {
    /// The case when the frame is a semantic one, it means that it corresponds
    /// to a frame in the resulting bytecode.
    Semantic {
        /// Array representing all the slots in the current semantic frame,
        /// an array element is set to `true` when it is currently used.
        available_slots: [bool; u8::MAX as usize],

        /// Maximum number of slots that are occupied simultaneously.
        maximum_size: u8,

        /// Map of up-values available in this semantic frame.
        up_values: HashMap<String, UpValueData>,

        /// The slot from which to close local values in this frame if this
        /// frame has to do it, [`None`] otherwise.
        close_from: Option<u8>,
    },

    /// The case when the frame is only existing in the intermediate
    /// representation. It is not kept in the resulting bytecode and all slots
    /// of this kind of frame are stored in the nearest parent frame that have
    /// the [`FrameVariant::Semantic`] variant.
    Lexical,
}

impl Frame {
    // --- Creation

    /// Create new empty semantic frame with an optional parent frame.
    pub fn new(parent_frame: Option<Rc<RefCell<Frame>>>) -> Self {
        Frame {
            parent_frame,
            bindings: HashMap::new(),
            variant: FrameVariant::Semantic {
                available_slots: [false; u8::MAX as usize],
                maximum_size: 0,
                up_values: HashMap::new(),
                close_from: None,
            },
        }
    }

    /// Create a new empty lexical frame with the given parent frame (a lexical
    /// frame can not exists without any parent).
    pub fn new_lexical(parent_frame: Rc<RefCell<Frame>>) -> Self {
        Frame {
            parent_frame: Some(parent_frame),
            bindings: HashMap::new(),
            variant: FrameVariant::Lexical,
        }
    }

    // --- Locals

    /// Get the data associated to the provided name in the current semantic
    /// frame if any.
    pub fn get_local(&self, name: &str) -> Option<BindingData> {
        match &self.variant {
            FrameVariant::Semantic { .. } => self.bindings.get(name).cloned(),
            FrameVariant::Lexical => self
                .bindings
                .get(name)
                .cloned()
                .or(self.parent_frame().unwrap().get_local(name)),
        }
    }

    /// Get whether the provided name is conflicting with a local in the
    /// current frame, returning the associated data if so.
    pub fn is_conflicting(&self, name: &str) -> Option<BindingData> {
        self.bindings.get(name).cloned()
    }

    /// Add a new local value to this frame, updating the already registered
    /// one if any.
    pub fn bind_local(&mut self, name: &str, declaration_location: &SourceSection) {
        let local_slot = self.reserve_contiguous_slots(1).first;
        self.bindings.insert(
            String::from(name),
            BindingData::new(declaration_location.clone(), local_slot),
        );
    }

    /// Mark a the local value designated by the provided name as initialized.
    pub fn init_local(&mut self, name: &str, birth_label: Label) {
        let binding = self.bindings.get_mut(name).unwrap();
        binding.is_init = true;
        binding.birth_label = birth_label;
    }

    /// Mark a the local value designated by the provided name as initialized.
    pub fn init_local_with_debug_name(&mut self, name: &str, debug_name: &str, birth_label: Label) {
        let binding = self.bindings.get_mut(name).unwrap();
        let _ = binding.debug_name.insert(String::from(debug_name));
        binding.birth_label = birth_label;
        binding.is_init = true;
    }

    // --- Up-values

    /// Get the up-value associate to the provided name in the current semantic
    /// frame if any.
    pub fn get_up_value(&mut self, name: &str) -> Option<UpValueData> where {
        // If we are a lexical frame, we delegate the request to our parent
        if matches!(self.variant, FrameVariant::Lexical) {
            return self.parent_frame_mut().unwrap().get_up_value(name);
        }

        // Now we know we are in a semantic frame, we start by looking in the
        // frame up-values.
        if let FrameVariant::Semantic { up_values, .. } = &self.variant {
            if let Some(up_value) = up_values.get(name) {
                return Some(up_value.clone());
            }
        }

        // If the up-value is not already in the frame ones, we try searching
        // in the parent frame.
        let new_up_value_index = self.next_up_value_index();
        let maybe_new_up_value = self.parent_frame_mut().and_then(|mut parent_frame| {
            // First look in the parent's locals
            if let Some(parent_local) = parent_frame.get_local(name) {
                parent_frame.close_binding(name);
                Some(UpValueData {
                    declaration_location: parent_local.declaration_location.clone(),
                    debug_name: parent_local.debug_name.clone(),
                    index: new_up_value_index,
                    is_safe: parent_local.is_init,
                    target: UpValueTarget::ParentSlot(parent_local.slot),
                })
            }
            // Then, recursively looks in the parent's up-values
            else if let Some(parent_up_value) = parent_frame.get_up_value(name) {
                Some(UpValueData {
                    declaration_location: parent_up_value.declaration_location.clone(),
                    debug_name: parent_up_value.debug_name.clone(),
                    index: new_up_value_index,
                    is_safe: parent_up_value.is_safe,
                    target: UpValueTarget::ParentUpValue(parent_up_value.index),
                })
            } else {
                None
            }
        });

        // If we found a result in the parent frame we add it into the
        // up-values map to cache the result.
        if let Some(new_up_value) = &maybe_new_up_value {
            match &mut self.variant {
                FrameVariant::Semantic { up_values, .. } => {
                    up_values.insert(String::from(name), new_up_value.clone());
                }
                _ => unreachable!(),
            }
        }

        // Finally, return the result
        maybe_new_up_value
    }

    /// Ge the next available up-value index, panicking if there is no more.
    fn next_up_value_index(&self) -> u8 {
        match &self.variant {
            FrameVariant::Semantic { up_values, .. } => {
                assert!(up_values.len() <= u8::MAX as usize, "Too many up-values");
                up_values.len() as u8
            }
            FrameVariant::Lexical => self.parent_frame().unwrap().next_up_value_index(),
        }
    }

    /// Update the frame information to close the provided slot.
    fn close_slot(&mut self, slot: u8) {
        match &mut self.variant {
            FrameVariant::Semantic { close_from, .. } => {
                if close_from.is_none() || close_from.unwrap() > slot {
                    let _ = close_from.insert(slot);
                }
            }
            FrameVariant::Lexical => self.parent_frame_mut().unwrap().close_slot(slot),
        }
    }

    /// Update the frame information to close the slot associated to the
    /// provided name. This function assumes that the binding is present in the
    /// current frame.
    fn close_binding(&mut self, name: &str) {
        // Update the binding closing kind
        {
            let binding = self.bindings.get_mut(name).unwrap();
            if !binding.is_init {
                binding.closing_kind = ClosingKind::Unsafe;
            } else if binding.closing_kind == ClosingKind::None {
                binding.closing_kind = ClosingKind::Safe;
            }
        }

        // Mark the bound slot as closed
        self.close_slot(self.bindings.get(name).unwrap().slot);
    }

    // --- Temporary values

    /// Get an unnamed temporary slot to store working values.
    pub fn get_tmp(&mut self) -> u8 {
        self.reserve_contiguous_slots(1).first
    }

    /// Get a range of contiguous available slots, reserving all of them. This
    /// function panics if there are not enough available slots.
    pub fn reserve_contiguous_slots(&mut self, count: usize) -> SlotRange {
        self.get_slots(count, true, true)
    }

    /// Release the provided slot, making it free to use.
    pub fn release_slot(&mut self, slot: u8) {
        self.release_slots(SlotRange { first: slot, last: slot });
    }

    /// Release all slots in the provided range, making them free to use.
    pub fn release_slots(&mut self, slots: SlotRange) {
        match &mut self.variant {
            FrameVariant::Semantic { available_slots, .. } => {
                for slot in slots.range() {
                    available_slots[slot as usize] = false;
                }
            }
            FrameVariant::Lexical => self.parent_frame_mut().unwrap().release_slots(slots),
        }
    }

    // --- Utils

    /// Get the parent frame of this one, if any.
    pub fn parent_frame(&self) -> Option<Ref<Frame>> {
        self.parent_frame.as_ref().map(|f| f.borrow())
    }

    /// Get the parent frame of this one as mutable, if any.
    fn parent_frame_mut(&self) -> Option<RefMut<Frame>> {
        self.parent_frame.as_ref().map(|f| f.borrow_mut())
    }

    /// Get the next available slot without flagging it as occupied.
    pub fn peek_next_slot(&self) -> u8 {
        match self.variant {
            FrameVariant::Semantic { available_slots, .. } => {
                available_slots
                    .iter()
                    .enumerate()
                    .filter(|s: &(usize, &bool)| !s.1)
                    .next()
                    .unwrap()
                    .0 as u8
            }
            FrameVariant::Lexical => self.parent_frame().unwrap().peek_next_slot(),
        }
    }

    /// Get a contiguous range of `count` available slots, updating the frame
    /// according to the function arguments.
    /// The returned range's end is exclusive, meaning that it is not reserved
    /// for use.
    fn get_slots(&mut self, count: usize, reserve: bool, update_size: bool) -> SlotRange {
        match &mut self.variant {
            FrameVariant::Semantic { available_slots, maximum_size, .. } => {
                let mut start_bound = 0;
                for i in 0..available_slots.len() {
                    // We know that `start_bound..(i-1)` slots are available,
                    // we check if this is enough.
                    if i - start_bound == count {
                        if reserve {
                            for j in start_bound..i {
                                available_slots[j] = true;
                            }
                        }
                        if update_size && i as u8 > *maximum_size {
                            *maximum_size = i as u8;
                        }
                        return SlotRange { first: start_bound as u8, last: i as u8 - 1 };
                    }

                    // If the `i`th slot is occupied, we move the cursor on it
                    if available_slots[i] {
                        start_bound = i + 1;
                    }
                }
                panic!("Too many local values");
            }
            FrameVariant::Lexical => self
                .parent_frame_mut()
                .unwrap()
                .reserve_contiguous_slots(count),
        }
    }
}

/// This type represents a frame slot that is bound to a lexical symbol.
#[derive(Debug, Clone)]
pub struct BindingData {
    /// Where in the source this binding has been declared
    pub declaration_location: SourceSection,

    /// Some bindings may have a debug name. This is a name that is going to be
    /// used when emitting debug information for this local variable, allowing
    /// it to have different lexical and runtime names.
    pub debug_name: Option<String>,

    /// The frame slot that is bound
    pub slot: u8,

    /// A label corresponding to the instruction from which this binding is
    /// available.
    pub birth_label: Label,

    /// A label corresponding to the instruction from which this binding isn't
    /// available anymore.
    pub death_label: Label,

    /// Whether the slot has been initialized yet
    pub is_init: bool,

    /// The way this slot should be closed for children frames
    pub closing_kind: ClosingKind,
}

impl BindingData {
    fn new(declaration_location: SourceSection, slot: u8) -> Self {
        Self {
            declaration_location,
            debug_name: None,
            slot,
            birth_label: usize::MAX,
            death_label: usize::MAX,
            is_init: false,
            closing_kind: ClosingKind::None,
        }
    }
}

/// This type represents ways a binding may be closed (marked as accessible for
/// children frames).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosingKind {
    /// The binding is not closed at all.
    None,

    /// The binding has been closed after its initialization.
    Safe,

    /// The binding has been closed before its initialization.
    Unsafe,
}

/// This type holds information about an up-value in a frame.
#[derive(Debug, Clone)]
pub struct UpValueData {
    /// Location of the declaration of the value ultimately targeted by this
    /// up-value.
    pub declaration_location: SourceSection,

    /// Some up-values may have a debug name. This is a name that is going to
    /// be used when emitting debug information for this up-value, allowing it
    /// to have different lexical and runtime names.
    pub debug_name: Option<String>,

    /// Index of the up-value in the current frame.
    pub index: u8,

    /// Whether the up-value is statically proved as initialized and can be
    /// safely read.
    pub is_safe: bool,

    /// What is the up-value targeting.
    pub target: UpValueTarget,
}

/// This type represents kinds of target that an up-value may have.
#[derive(Debug, Clone, Copy)]
pub enum UpValueTarget {
    /// When the up-value to read is a slot of the parent frame.
    ParentSlot(u8),

    /// When the up-value to read is an up-value of the parent frame.
    ParentUpValue(u8),
}

/// This type represents a range of slots, bounds included.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SlotRange {
    pub first: u8,
    pub last: u8,
}

impl SlotRange {
    /// Get a standard range from this slot range to iterate over all slots in
    /// the range.
    fn range(&self) -> Range<u8> {
        self.first..self.last + 1
    }
}
