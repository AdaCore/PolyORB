with System;

with CORBA.Object;
with CORBA.BOA;

--  with CORBA.NVList;
--  with CORBA.OperationDef;
--  with CORBA.Context;
--  with CORBA.Sequences;

with OmniObject;

package CORBA.ORB is

   ------------------------------
   -- CORBA 2.2 28 February 99 --
   ------------------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'class)
      return CORBA.String
     renames CORBA.Object.Object_To_String;

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'class)
     renames CORBA.Object.String_To_Object;

   --  not yet implemented
   --
   --    procedure Create_List
   --      (Count    : in     CORBA.Long;
   --       New_List :    out CORBA.NVList.Object);
   --
   --    procedure Create_Operation_List
   --      (Oper     : in     CORBA.OperationDef.Ref;
   --       New_List :    out CORBA.NVList.Object);
   --
   --    function Get_Default_Context return CORBA.Context.Object;

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
