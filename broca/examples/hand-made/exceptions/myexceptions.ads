with CORBA;
with CORBA.Object;
with Ada.Exceptions;
pragma Elaborate_All (CORBA);

package myexceptions is

   type Ref is new CORBA.Object.Ref with null record;

   function Unchecked_To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref;

   function To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref;

   toto : exception;

   type toto_Members is new CORBA.IDL_Exception_Members with
   record
      i : CORBA.Long;
   end record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out toto_Members);

   Repository_Id : constant CORBA.String
      := CORBA.To_CORBA_String ("IDL:myexceptions:1.0");

   function Is_A
     (Self   : Ref;
      Logical_Type_Id : CORBA.String)
      return CORBA.Boolean;

end myexceptions;
