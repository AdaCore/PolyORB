with CORBA;
with CORBA.Object;
pragma Elaborate_All (CORBA);

package Echo is

   type Ref is new CORBA.Object.Ref with null record;

   function Unchecked_To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref;

   function To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref;

   function echoString
     (Self : in Ref;
      Mesg : in CORBA.String)
      return CORBA.String;

   Repository_Id : constant CORBA.String
      := CORBA.To_CORBA_String ("IDL:Echo:1.0");

   function Is_A
     (Self   : Ref;
      Logical_Type_Id : CORBA.String)
      return CORBA.Boolean;

end Echo;
