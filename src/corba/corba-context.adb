--  $Id$

package body CORBA.Context is

   procedure Set_One_Value
     (Self      : in Ref;
      Prop_Name : in Identifier;
      Value     : in CORBA.String) is
   begin
      null;
   end Set_One_Value;

   procedure Set_Values
     (Self   : in Ref;
      Values : in CORBA.NVList.Ref) is
   begin
      null;
   end Set_Values;

   procedure Get_Values
     (Self        : in     Ref;
      Start_Scope : in     Identifier;
      This_Object : in     Boolean := True;
      Prop_Name   : in     Identifier;
      Values      :    out CORBA.NVList.Ref)
   is
      Dummy : CORBA.NVList.Ref;
      pragma Warnings (Off, Dummy);
   begin
      Values := Dummy;
   end Get_Values;

   procedure Delete_Values
     (Self      : in Ref;
      Prop_Name : in Identifier) is
   begin
      null;
   end Delete_Values;

   procedure Create_Child
     (Self      : in     Ref;
      Ctx_Name  : in     Identifier;
      Child_Ctx :    out Ref)
   is
      Dummy : Ref;
      pragma Warnings (Off, Dummy);
   begin
      Child_Ctx := Dummy;
   end Create_Child;

   procedure Delete
     (Self       : in Ref;
      Del_Flagfs : in Flags) is
   begin
      null;
   end Delete;

end CORBA.Context;
