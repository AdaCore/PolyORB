------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.GIOP_P.TAGGED_COMPONENTS.ALTERNATE_IIOP_ADDRESS          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Representations.CDR.Common;

package body PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address is

   use PolyORB.Representations.CDR.Common;
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
   begin
      Free (C.Address);
   end Release_Contents;

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   procedure Marshall_Component_Data
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type)
   is
      Temp_Buf : Buffer_Access := new Buffer_Type;

   begin
      Start_Encapsulation (Temp_Buf);
      Marshall_Socket (Temp_Buf, C.Address.all);
      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   procedure Unmarshall_Component_Data
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use PolyORB.Errors;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);

      Temp_Buf : Buffer_Access := new Buffer_Type;
   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);

      C.Address := new Socket_Name'(Unmarshall_Socket (Temp_Buf));

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
               Release (Temp_Buf);
               Throw (Error,
                      Bad_Param_E,
                      System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (C : TC_Alternate_IIOP_Address)
     return Tagged_Component_Access
   is
      Result : constant Tagged_Component_Access :=
                 new TC_Alternate_IIOP_Address;
   begin
      TC_Alternate_IIOP_Address (Result.all).Address :=
        new Socket_Name'(C.Address.all);

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
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
