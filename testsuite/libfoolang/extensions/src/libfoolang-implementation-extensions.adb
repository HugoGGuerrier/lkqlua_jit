with Langkit_Support.Text; use Langkit_Support.Text;

package body Libfoolang.Implementation.Extensions is

   ---------------------------------------
   -- Foo_Node_P_Trigger_Unit_Requested --
   ---------------------------------------

   function Foo_Node_P_Trigger_Unit_Requested
     (Node  : Bare_Foo_Node;
      Name  : Symbol_Type;
      Found : Boolean;
      Error : Boolean) return Boolean
   is
      EH : constant Internal_Event_Handler_Access :=
        Node.Unit.Context.Event_Handler;
   begin
      if EH /= null then
         EH.Unit_Requested_Callback
           (Context            => Node.Unit.Context,
            Name               => +Name,
            From               => Node.Unit,
            Found              => Found,
            Is_Not_Found_Error => Error);
      end if;

      return False;
   end Foo_Node_P_Trigger_Unit_Requested;

   -------------------------
   -- Foo_Node_P_Get_Sloc --
   -------------------------

   function Foo_Node_P_Get_Sloc
     (Node : Bare_Foo_Node; Line : Integer; Col : Integer)
      return Source_Location is
   begin
      return Source_Location'(Line_Number (Line), Column_Number (Col));
   end Foo_Node_P_Get_Sloc;

   -----------------------------------
   -- Foo_Node_P_Get_Line_From_Sloc --
   -----------------------------------

   function Foo_Node_P_Get_Line_From_Sloc
     (Node : Bare_Foo_Node; Sloc : Source_Location) return Integer is
   begin
      return Integer (Sloc.Line);
   end Foo_Node_P_Get_Line_From_Sloc;

   ----------------------------------
   -- Foo_Node_P_Get_Col_From_Sloc --
   ----------------------------------

   function Foo_Node_P_Get_Col_From_Sloc
     (Node : Bare_Foo_Node; Sloc : Source_Location) return Integer is
   begin
      return Integer (Sloc.Column);
   end Foo_Node_P_Get_Col_From_Sloc;

   -------------------------------
   -- Foo_Node_P_Get_Sloc_Array --
   -------------------------------

   function Foo_Node_P_Get_Sloc_Array
     (Node : Bare_Foo_Node) return Source_Location_Array_Access is
   begin
      return Create_Source_Location_Array
        (((1, 2), (3, 4)));
   end Foo_Node_P_Get_Sloc_Array;

end Libfoolang.Implementation.Extensions;
