with System;

with CORBA.Object;
with CORBA.BOA;

--  with CORBA.NVList;
--  with CORBA.OperationDef;
--  with CORBA.Context;
--  with CORBA.Sequences;

with AdaBroker.OmniObject;

package CORBA.ORB is

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String;
   --  Returns the IOR corresponding to this object it is called by
   --  CORBA.ORB.Object_To_String see CORBA specification for details

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Returns a Ref'Class out of an IOR it is called by
   --  CORBA.ORB.String_To_Object see CORBA specification for details

   ---------------
   -- AdaBroker --
   ---------------

   type Object is new System.Address;

   function Object_To_String
     (Obj : in OmniObject.Implemented_Object'class)
      return CORBA.String
     renames OmniObject.Object_To_String;
   --  string object_to_string (in Object obj); server-side

   --    procedure Resolve_Initial_References
   --      (Identifier : in Standard.String;
   --       Result     : out CORBA.Object.Ref'class);

   ------------------------
   -- ORB initialization --
   ------------------------

   function ORB_Init
     (ORB_Name : in Standard.String)
      return Object;
   --  Initializes the ORB with parameters of the command line and returns
   --  the ORB

   function BOA_Init
     (Self     : in Object;
      BOA_Name : in Standard.String)
      return CORBA.BOA.Object;
   --  Initializes the BOA with parameters of the command line and returns
   --  the BOA

end CORBA.ORB;
