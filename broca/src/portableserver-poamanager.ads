with Ada.Exceptions;
with CORBA;
with CORBA.Object;

package PortableServer.POAManager is
   type Ref is new CORBA.Object.Ref with null record;

   AdapterInactive : exception;

   type AdapterInactive_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                          To   : out AdapterInactive_Members);

   procedure Activate (Self : in Ref);

   procedure Hold_Requests
     (Self : in Ref;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Discard_Requests
     (Self : in Ref;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Deactivate
     (Self : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean);
end PortableServer.POAManager;
