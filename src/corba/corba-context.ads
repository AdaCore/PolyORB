--  $Id$

with CORBA.NVList;

package CORBA.Context is

   type Ref is private;
   Nil_Ref : constant Ref;

   procedure Set_One_Value
     (Self      : in Ref;
      Prop_Name : in Identifier;
      Value     : in CORBA.String);

   procedure Set_Values
     (Self   : in Ref;
      Values : in CORBA.NVList.Ref);

   procedure Get_Values
     (Self        : in     Ref;
      Start_Scope : in     Identifier;
      This_Object : in     Boolean := True;
      Prop_Name   : in     Identifier;
      Values      :    out CORBA.NVList.Ref);

   procedure Delete_Values
     (Self      : in Ref;
      Prop_Name : in Identifier);

   procedure Create_Child
     (Self      : in     Ref;
      Ctx_Name  : in     Identifier;
      Child_Ctx :    out Ref);

   procedure Delete
     (Self       : in Ref;
      Del_Flagfs : in Flags);

private

   type Ref is null record;
   Nil_Ref : constant Ref := (null record);


end CORBA.Context;
