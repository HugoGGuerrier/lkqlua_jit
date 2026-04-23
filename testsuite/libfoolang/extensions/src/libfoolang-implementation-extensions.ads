package Libfoolang.Implementation.Extensions is

   function Foo_Node_P_Trigger_Unit_Requested
     (Node  : Bare_Foo_Node;
      Name  : Symbol_Type;
      Found : Boolean;
      Error : Boolean) return Boolean;

   function Foo_Node_P_Get_Sloc
     (Node : Bare_Foo_Node; Line : Integer; Col : Integer)
      return Source_Location;

   function Foo_Node_P_Get_Line_From_Sloc
     (Node : Bare_Foo_Node; Sloc : Source_Location) return Integer;

   function Foo_Node_P_Get_Col_From_Sloc
     (Node : Bare_Foo_Node; Sloc : Source_Location) return Integer;

   function Foo_Node_P_Get_Sloc_Array
     (Node : Bare_Foo_Node) return Source_Location_Array_Access;

end Libfoolang.Implementation.Extensions;
