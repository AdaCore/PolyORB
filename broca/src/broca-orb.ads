with CORBA;
with CORBA.ORB;
with CORBA.Object;
with Broca.POA;
with Broca.Buffers;
pragma Elaborate_All (CORBA);

package Broca.ORB is

   procedure IOR_To_Object
     (IOR : in out Broca.Buffers.Buffer_Descriptor;
      Ref : out CORBA.Object.Ref'Class);

   function List_Initial_Services
     return CORBA.ORB.ObjectIdList;

   function Resolve_Initial_References
     (Identifier : CORBA.ORB.ObjectId)
     return CORBA.Object.Ref;

   procedure Register_Initial_Reference
     (Identifier : in CORBA.ORB.ObjectId;
      Reference  : in CORBA.Object.Ref);

   type ORB_Type is abstract tagged null record;
   --  Internal tricks to avoid to a client to contain any part of the
   --  server. The server part must register itself (only one server
   --  is allowed).

   procedure Run (ORB : in out ORB_Type) is abstract;

   --  A state of a poa has changed.
   procedure POA_State_Changed
     (ORB : in out ORB_Type;
      POA : in Broca.POA.POA_Object_Access) is abstract;

   type ORB_Access is access all ORB_Type'Class;

   procedure Register_ORB (ORB : ORB_Access);
   procedure Run;
   procedure POA_State_Changed (POA : Broca.POA.POA_Object_Access);

   --  Well Known ObjectIds.
   Root_POA_ObjectId             : constant CORBA.ORB.ObjectId;
   POA_Current_Objectid          : constant CORBA.ORB.ObjectId;
   Interface_Repository_ObjectId : constant CORBA.ORB.ObjectId;

private

   Root_POA_ObjectId : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.ObjectId (CORBA.String'(CORBA.To_CORBA_String ("RootPOA")));

   POA_Current_Objectid : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.ObjectId (CORBA.String'(CORBA.To_CORBA_String ("POACurrent")));

   Interface_Repository_Objectid : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.ObjectId
     (CORBA.String'(CORBA.To_CORBA_String ("InterfaceRepository")));

end Broca.ORB;
