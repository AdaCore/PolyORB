------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.GIOP_P.TAGGED_COMPONENTS.ALTERNATE_IIOP_ADDRESS          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Initialization;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address is

   use PolyORB.Utils.Sockets;

   function Create_Empty_Component return Tagged_Component_Access;

--   function Fetch_Component
--     (Oid : access PolyORB.Objects.Object_Id)
--      return Tagged_Component_Access;
--
--  Alternate_IIOP_Address tag created by IIOP Transport Mechanism factory,
--  thus no fetch function needed.

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_Alternate_IIOP_Address;
   end Create_Empty_Component;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (C : access TC_Alternate_IIOP_Address) is
      pragma Unreferenced (C);
   begin
      null;
   end Release_Contents;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type)
   is
   begin
      Marshall_Socket (Buffer, C.Address);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type)
   is
   begin
      Unmarshall_Socket (Buffer, C.Address);
   end Unmarshall;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (C : TC_Alternate_IIOP_Address)
     return Tagged_Component_Access
   is
      Result : constant Tagged_Component_Access
        := new TC_Alternate_IIOP_Address;
   begin
      TC_Alternate_IIOP_Address (Result.all).Address := C.Address;

      return Result;
   end Duplicate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register
        (Tag_Alternate_IIOP_Address,
         Create_Empty_Component'Access,
         null);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.alternate_iiop_address",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
