with CORBA;
with CORBA.ORB;
with CORBA.Object;
with Broca.POA;
with Broca.Buffers;
pragma Elaborate_All (CORBA);

package Broca.ORB is
   procedure IOR_To_Object (IOR : in out Broca.Buffers.Buffer_Descriptor;
                            Res : out CORBA.Object.Ref'Class);

   function List_Initial_Services return CORBA.ORB.ObjectIdList;

   function Resolve_Initial_References (Identifier : CORBA.ORB.ObjectId)
                                        return CORBA.Object.Ref;

   procedure Register_Initial_Reference
     (Identifier : CORBA.ORB.ObjectId; Ref : CORBA.Object.Ref);

   --  Internal tricks to avoid to a client to contain any part of the server.
   --  The server part must register itself (only one server is allowed).
   type Orb_Type is abstract tagged null record;
   procedure Run (ORB : in out Orb_Type) is abstract;
   --  A state of a poa has changed.
   procedure Poa_State_Changed
     (ORB : in out Orb_Type; POA : Broca.POA.POA_Object_Access) is
      abstract;

   type Orb_Access is access all Orb_Type'Class;

   procedure Register_Orb (ORB : Orb_Access);
   procedure Run;
   procedure Poa_State_Changed (POA : Broca.POA.POA_Object_Access);

   --  Well known ObjectIds.
   Root_Poa_Objectid : constant CORBA.ORB.ObjectId;
   Poa_Current_Objectid : constant CORBA.ORB.ObjectId;
   Interface_Repository_Objectid : constant CORBA.ORB.ObjectId;

private
   Root_Poa_Objectid : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.ObjectId (CORBA.String'(CORBA.To_CORBA_String ("RootPOA")));
   Poa_Current_Objectid : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.ObjectId (CORBA.String'(CORBA.To_CORBA_String ("POACurrent")));
   Interface_Repository_Objectid : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.ObjectId
     (CORBA.String'(CORBA.To_CORBA_String ("InterfaceRepository")));
end Broca.ORB;
