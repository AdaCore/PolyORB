------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.22 $
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

with Interfaces.C.Strings;

with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

with CORBA.Command_Line;
with CORBA.Object;
with CORBA.ORB.OmniORB;
with CORBA.Object.OmniORB;

package body CORBA.ORB is

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("corba.orb");
   procedure O is new AdaBroker.Debug.Output (Flag);

   function C_ORB_Init
     (Argc    : in Interfaces.C.int;
      Argv    : in System.Address;
      ORBname : in Interfaces.C.Strings.chars_ptr)
      return System.Address;

   pragma Import (CPP, C_ORB_Init, "Ada_ORB_init__FiPPcPCc");
   --  Wrapper around Ada_ORB_init. See Ada_CORBA_ORB.hh.

   ----------
   -- Init --
   ----------

   procedure Init
     (Identifier : in Standard.String)
   is
      C_Identifier : Interfaces.C.Strings.chars_ptr;
   begin
      pragma Debug (O ("Init : enter"));
      pragma Debug (O ("ORB name is " & Identifier));

      C_Identifier := Interfaces.C.Strings.New_String (Identifier);

      pragma Debug (O ("ORB_Init : invoke CORBA::ORB_init"));

      CORBA.ORB.OmniORB.The_ORB := C_ORB_Init (CORBA.Command_Line.Argc,
                                               CORBA.Command_Line.Argv,
                                               C_Identifier);

      pragma Debug (O ("Init : leave"));

      Interfaces.C.Strings.Free (C_Identifier);
   end Init;

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

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : in ObjectId)
     return CORBA.Object.Ref
   is
   begin
      return CORBA.Object.OmniORB.Resolve_Initial_References
        (CORBA.String (Identifier));
   end Resolve_Initial_References;

end CORBA.ORB;
