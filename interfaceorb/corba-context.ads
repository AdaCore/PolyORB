package CORBA.Context is

   type Object is limited private;

   procedure Set_One_Value
     (Self      : in out Object;
      Prop_Name : in    Identifier;
      Value     : in    CORBA.String);

   procedure Set_Values
     (Self      : in out Object;
      Values    : in     CORBA.NVList.Object);

   procedure Get_Values
     (Self        : in     Object;
      Start_Scope : in     Identifier;
      This_Object : in     Boolean := TRUE;
      Prop_Name   : in     Identifier;
      Values      :    out CORBA.NVList.Object);

   procedure Delete_Values
     (Self      : in out Object;
      Prop_Name : in     Identifier);

   procedure Create_Child
     (Self      : in out Object;
      Ctx_Name  : in     Identifier;
      Child_Ctx :    out Object);

   procedure Delete
     (Self              : in Object;
      Delete_Descendant : in Boolean := FALSE);

private
   -- implementation defined

end CORBA.Context;
