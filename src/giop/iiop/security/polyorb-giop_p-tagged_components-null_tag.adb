------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TAGGED_COMPONENTS.NULL_TAG                 --
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

pragma Ada_2005;

with PolyORB.Binding_Data.GIOP;
with PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
with PolyORB.GIOP_P.Transport_Mechanisms;
with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Security.Transport_Mechanisms.Unprotected;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Null_Tag is

   use PolyORB.Representations.CDR.Common;
   use PolyORB.GIOP_P.Transport_Mechanisms;

   function Create_Empty_Component return Tagged_Component_Access;

   procedure Initialize;

   function To_Tagged_Component
     (TM : PolyORB.Security.Transport_Mechanisms.
       Target_Transport_Mechanism_Access)
     return Tagged_Component_Access;

   function To_Security_Transport_Mechanism
     (TC : access Tagged_Component'Class)
      return
       PolyORB.Security.Transport_Mechanisms.Client_Transport_Mechanism_Access;

   procedure Create_GIOP_Transport_Mechanisms
     (TC      : PolyORB.GIOP_P.Tagged_Components.Tagged_Component_Access;
      Profile : PolyORB.Binding_Data.Profile_Access;
      Mechs   : in out Transport_Mechanism_List);

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_Null_Tag;
   end Create_Empty_Component;

   --------------------------------------
   -- Create_GIOP_Transport_Mechanisms --
   --------------------------------------

   procedure Create_GIOP_Transport_Mechanisms
     (TC      : PolyORB.GIOP_P.Tagged_Components.Tagged_Component_Access;
      Profile : PolyORB.Binding_Data.Profile_Access;
      Mechs   : in out Transport_Mechanism_List)
   is
      pragma Unreferenced (TC);
      use PolyORB.Binding_Data.GIOP;
   begin
      Append (Mechs,
        Get_Primary_Transport_Mechanism (GIOP_Profile_Type (Profile.all)));
   end Create_GIOP_Transport_Mechanisms;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (C : TC_Null_Tag)
     return Tagged_Component_Access
   is
      pragma Unreferenced (C);

   begin
      return new TC_Null_Tag;
   end Duplicate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Register Tagged Component

      Register (Tag_NULL_Tag, Create_Empty_Component'Access, null);

      --  Register Tagged Component => GIOP Transport Mechanisms convertor

      PolyORB.GIOP_P.Transport_Mechanisms.Register
        (Tag_NULL_Tag, Create_GIOP_Transport_Mechanisms'Access);

      --  Register Tagged Component <=> Secure Transport Mechanism convertor

      PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List.Register
        (Tag_NULL_Tag,
         To_Tagged_Component'Access,
         To_Security_Transport_Mechanism'Access);
   end Initialize;

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   overriding procedure Marshall_Component_Data
     (C      : access TC_Null_Tag;
      Buffer : access Buffer_Type)
   is
      pragma Unreferenced (C);

   begin
      Marshall (Buffer, Types.Unsigned_Long (0));
   end Marshall_Component_Data;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents (C : access TC_Null_Tag) is
      pragma Unreferenced (C);
   begin
      null;
   end Release_Contents;

   -------------------------------------
   -- To_Security_Transport_Mechanism --
   -------------------------------------

   function To_Security_Transport_Mechanism
     (TC : access Tagged_Component'Class)
      return
       PolyORB.Security.Transport_Mechanisms.Client_Transport_Mechanism_Access
   is
      pragma Unreferenced (TC);

      package PSTMU
        renames PolyORB.Security.Transport_Mechanisms.Unprotected;

   begin
      return new PSTMU.Unprotected_Transport_Mechanism;
   end To_Security_Transport_Mechanism;

   -------------------------
   -- To_Tagged_Component --
   -------------------------

   function To_Tagged_Component
     (TM : PolyORB.Security.Transport_Mechanisms.
       Target_Transport_Mechanism_Access)
     return Tagged_Component_Access
   is
      pragma Unreferenced (TM);

   begin
      return null;
   end To_Tagged_Component;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_Null_Tag;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);

      use PolyORB.Types;

      Aux : constant Unsigned_Long := Unmarshall (Buffer);

   begin
      pragma Assert (Aux = 0);

      null;
   end Unmarshall_Component_Data;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"tagged_components.null_tag",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => PolyORB.Initialization.String_Lists.Empty,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Tagged_Components.Null_Tag;
