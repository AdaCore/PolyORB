with System;

with CORBA.Object;
with CORBA.BOA;

package CORBA.ORB is

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String;
   --  Return a IOR corresponding to this object.

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Return a Ref'Class out of an IOR.

   --  function Resolve_Initial_References
   --    (Identifier : in CORBA.String)
   --    return CORBA.Object.Ref;

   ---------------
   -- AdaBroker --
   ---------------

   type Object is private;

   function ORB_Init
     (ORB_Name : in Standard.String)
      return Object;
   --  Initialize ORB with command line arguments and return ORB

   function BOA_Init
     (Self     : in Object;
      BOA_Name : in Standard.String)
      return CORBA.BOA.Object;
   --  Initialize BOA with command line arguments and return BOA

private

   type Object is new System.Address;

end CORBA.ORB;
