with Interfaces.C.Strings;

with System;

with AdaBroker; use AdaBroker;
with AdaBroker.OmniObject;
with AdaBroker.Debug;
pragma Elaborate (AdaBroker.Debug);

with CORBA.Command_Line;
with CORBA.Object;
with CORBA.Object.OmniORB;

package body CORBA.ORB is

   use type OmniObject.Object_Ptr;

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("corba.orb");
   procedure O is new AdaBroker.Debug.Output (Flag);

   function C_ORB_Init
     (Argc    : in Interfaces.C.int;
      Argv    : in System.Address;
      ORBname : in Interfaces.C.Strings.chars_ptr)
      return Object;

   pragma Import (CPP, C_ORB_Init, "Ada_ORB_init__FiPPcPCc");
   --  Wrapper around Ada_ORB_init. See Ada_CORBA_ORB.hh.

   --------------
   -- ORB_Init --
   --------------

   function ORB_Init
     (ORB_Name : in Standard.String)
      return Object
   is
      C_ORB_Name : Interfaces.C.Strings.chars_ptr;
      Result     : Object;
   begin
      pragma Debug (O ("-- CORBA.ORB.ORB_Init --"));
      pragma Debug (O (ORB_Name));

      C_ORB_Name := Interfaces.C.Strings.New_String (ORB_Name);
      --  Deallocated 4 lines further

      pragma Debug (O ("ORB_Init : calling CORBA::ORB_init"));

      Result := C_ORB_Init (CORBA.Command_Line.Argc,
                            CORBA.Command_Line.Argv,
                            C_ORB_Name);

      pragma Debug (O ("ORB_Init : ORB initialized"));

      Interfaces.C.Strings.Free (C_ORB_Name);
      return Result;
   end ORB_Init;

   ----------------
   -- C_BOA_Init --
   ----------------

   function C_BOA_Init
     (Self    : in Object;
      Argc    : in Interfaces.C.int;
      Argv    : in System.Address;
      Boaname : in Interfaces.C.Strings.chars_ptr)
      return CORBA.BOA.Object;

   pragma Import (CPP, C_BOA_Init, "Ada_BOA_init__FPQ25CORBA3ORBiPPcPCc");
   --  Calls Ada_Boa_Init. See Ada_CORBA_ORB.hh.

   --------------
   -- BOA_Init --
   --------------

   function BOA_Init
     (Self     : in Object;
      BOA_Name : in Standard.String)
      return CORBA.BOA.Object
   is
      C_BOA_Name : Interfaces.C.Strings.chars_ptr;
      Result     : CORBA.BOA.Object;
   begin
      C_BOA_Name := Interfaces.C.Strings.New_String (BOA_Name);
      --  Deallocated 4 lines further

      Result := C_BOA_Init
        (Self,
         CORBA.Command_Line.Argc,
         CORBA.Command_Line.Argv,
         C_BOA_Name);

      Interfaces.C.Strings.Free (C_BOA_Name);
      return Result;
   end BOA_Init;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String
   is
   begin
      return CORBA.Object.OmniORB.Object_To_String (Obj);
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class)
   is
   begin
      CORBA.Object.OmniORB.String_To_Object (From, To);
   end String_To_Object;

end CORBA.ORB;
