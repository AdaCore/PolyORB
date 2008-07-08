with CORBA.Object;
with CORBA;

package Harness is

   type Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant Standard.String :=
     "IDL:Harness:1.0";

   function echoULong
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   function Is_A
     (Self : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean;

private
   function Is_A
     (Logical_Type_Id : in Standard.String)
     return CORBA.Boolean;

end Harness;
