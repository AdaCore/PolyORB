with CORBA;
with CORBA.ORB;
with CORBA.Object;
with Broca.Poa;
with Broca.Types;
pragma Elaborate_All (CORBA);

package Broca.Orb is
   procedure IOR_To_Object (Ior : in out Broca.Types.Buffer_Descriptor;
                            Res : out CORBA.Object.Ref'Class);

   function List_Initial_Services return CORBA.ORB.ObjectIdList;

   function Resolve_Initial_References (Identifier : CORBA.ORB.ObjectId)
                                        return CORBA.Object.Ref;

   procedure Register_Initial_Reference
     (Identifier : CORBA.ORB.ObjectId; Ref : CORBA.Object.Ref);

   --  Internal tricks to avoid to a client to contain any part of the server.
   --  The server part must register itself (only one server is allowed).
   type Orb_Type is abstract tagged null record;
   procedure Run (Orb : in out Orb_Type) is abstract;
   --  A state of a poa has changed.
   procedure Poa_State_Changed
     (Orb : in out Orb_Type; Poa : Broca.Poa.POA_Object_Access) is
      abstract;

   type Orb_Access is access all Orb_Type'Class;

   procedure Register_Orb (Orb : Orb_Access);
   procedure Run;
   procedure Poa_State_Changed (Poa : Broca.Poa.POA_Object_Access);

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
end Broca.Orb;
