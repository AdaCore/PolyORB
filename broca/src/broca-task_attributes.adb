with Ada.Task_Attributes;

package body Broca.Task_Attributes is

   type Task_Attribute is record
      Current_Object    : PortableServer.ObjectId;
      Current_POA       : PortableServer.POA_Forward.Ref;
   end record;

   Nil_Attribute : Task_Attribute;

   package Attributes is new Ada.Task_Attributes
     (Attribute => Task_Attribute,
      Initial_Value => Nil_Attribute);

   function Current_Object return PortableServer.ObjectId is
   begin
      return Attributes.Value.Current_Object;
   end Current_Object;

   function Current_POA return PortableServer.POA_Forward.Ref is
   begin
      return Attributes.Value.Current_POA;
   end Current_POA;

   procedure Set_Current_Object (Val : PortableServer.ObjectId) is
      Current_Attributes : Task_Attribute := Attributes.Value;
   begin
      Current_Attributes.Current_Object := Val;
      Attributes.Set_Value (Current_Attributes);
   end Set_Current_Object;

   procedure Set_Current_POA (Val : PortableServer.POA_Forward.Ref) is
      Current_Attributes : Task_Attribute := Attributes.Value;
   begin
      Current_Attributes.Current_POA := Val;
      Attributes.Set_Value (Current_Attributes);
   end Set_Current_POA;

end Broca.Task_Attributes;
