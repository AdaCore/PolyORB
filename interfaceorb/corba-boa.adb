------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . B O A                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package is wrapped around a C++ class whose name is BOA declared
--  in file CORBA.h.  It provides two types of methods : the C functions of
--  the BOA class and their equivalent in Ada. (the first ones have a C_
--  prefix.)

with AdaBroker.Sysdep;  use AdaBroker.Sysdep;
with AdaBroker.OmniORB;
with Interfaces.C;
with Interfaces.C.Strings;
with CORBA.ORB.OmniORB;
with CORBA.Command_Line;

package body CORBA.BOA is

   procedure Impl_Shutdown (Self : in System.Address);
   pragma Import (CPP, Impl_Shutdown, "impl_shutdown__FPQ25CORBA3BOA");

   procedure Destroy (Self : in System.Address);
   pragma Import (CPP, Destroy, "destroy__FPQ25CORBA3BOA");

   ----------------
   -- C_BOA_Init --
   ----------------

   function C_BOA_Init
     (The_ORB : in System.Address;
      Argc    : in Interfaces.C.int;
      Argv    : in System.Address;
      BOAName : in Interfaces.C.Strings.chars_ptr)
      return System.Address;

   pragma Import (CPP, C_BOA_Init, "Ada_BOA_init__FPQ25CORBA3ORBiPPcPCc");
   --  Calls Ada_Boa_Init. See Ada_CORBA_ORB.hh.

   The_BOA : System.Address;

   --------------
   -- Init --
   --------------

   procedure Init
     (Identifier : in Standard.String)
   is
      C_Identifier : Interfaces.C.Strings.chars_ptr;
   begin
      C_Identifier := Interfaces.C.Strings.New_String (Identifier);

      The_BOA := C_BOA_Init
        (CORBA.ORB.OmniORB.The_ORB,
         CORBA.Command_Line.Argc,
         CORBA.Command_Line.Argv,
         C_Identifier);

      Interfaces.C.Strings.Free (C_Identifier);
   end Init;

   ---------------------
   -- C_Impl_Is_Ready --
   ---------------------

   procedure C_Impl_Is_Ready
     (The_BOA      : in System.Address;
      Impl         : in System.Address;
      Non_Blocking : in Bool);
   pragma Import
     (CPP, C_Impl_Is_Ready,
      "impl_is_ready__FPQ25CORBA3BOAPQ25CORBA17ImplementationDefb");
   --  Corresponds to Ada_CORBA_Boa method impl is ready see
   --  Ada_CORBA_Boa.hh

   -------------------
   -- Impl_Is_Ready --
   -------------------

   procedure Impl_Is_Ready
     (Non_Blocking : in Boolean := False)
   is
      Tmp : System.Address    := System.Null_Address;
      NB  : Bool := To_Bool (Non_Blocking);
   begin
      C_Impl_Is_Ready (The_BOA, Tmp, NB);
   end Impl_Is_Ready;

   ---------------------
   -- Object_Is_Ready --
   ---------------------

   procedure Object_Is_Ready
     (Obj : in AdaBroker.OmniORB.ImplObject'Class)
   is
   begin
      AdaBroker.OmniORB.Initialize_Local_Object (Obj);

      --  It does not take the BOA into account because there is only
      --  one BOA in omniORB2. (See corbaBoa.cc)
      AdaBroker.OmniORB.Object_Is_Ready (Obj);
   end Object_Is_Ready;

   --------------------
   -- Dispose_Object --
   --------------------

   procedure Dispose_Object
     (Obj : in AdaBroker.OmniORB.ImplObject'Class)
   is
   begin
      --  It does not take the BOA into account because there is only
      --  one BOA in omniORB2. (See corbaBoa.cc)
      AdaBroker.OmniORB.Dispose_Object (Obj);
   end Dispose_Object;

   -------------------
   -- Impl_Shutdown --
   -------------------

   procedure Impl_Shutdown is
   begin
      Impl_Shutdown (The_BOA);
   end Impl_Shutdown;

   -------------
   -- Destroy --
   -------------

   procedure Destroy is
   begin
      Destroy (The_BOA);
   end Destroy;

end CORBA.BOA;
