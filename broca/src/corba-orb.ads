with CORBA; use CORBA;
with CORBA.Object;
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

--  with CORBA.BOA;

--  with CORBA.NVList;
--  with CORBA.OperationDef;
--  with CORBA.Context;
--  with CORBA.Sequences;

package CORBA.ORB is

   --  21.34

--    type Octet_Sequence is new CORBA.Sequences.Unbounded (Octet);
--    type ServiceDetail is record
--       Service_Detail_Type: ServiceDetailType;
--       Service_Detail: Octet_Sequence.Sequence;
--    end record;

   type ObjectId is new CORBA.String;
   package IDL_SEQUENCE_ObjectId is new CORBA.Sequences.Unbounded (ObjectId);
   type ObjectIdList is new IDL_SEQUENCE_ObjectId.Sequence;

--    function Object_To_String
--      (Obj : in Broca.Imp_object.Implemented_Object'class)
--       return CORBA.String;

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Returns a Ref'Class out of an IOR it is called by
   --  CORBA.ORB.String_To_Object see CORBA specification for details

   --  Obtaining initial object references.

   function List_Initial_Services return ObjectIdList;

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref;

   procedure Run;

   ------------------------
   -- ORB initialization --
   ------------------------

--    function ORB_Init
--      (ORB_Name : in Standard.String)
--       return Object;
--    --  Initializes the ORB with parameters of the command line and returns
--    --  the ORB

--    function BOA_Init
--      (Self     : in Object;
--       BOA_Name : in Standard.String)
--       return CORBA.BOA.Object;
--    --  Initializes the BOA with parameters of the command line and returns
--    --  the BOA

end CORBA.ORB;
