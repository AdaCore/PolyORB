------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . G I O P . U I P M C       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  Binding data concrete implementation for UIPMC

with PolyORB.Buffers;
with PolyORB.MIOP_P.Groups;
with PolyORB.Types;

package PolyORB.Binding_Data.GIOP.UIPMC is

   use PolyORB.Buffers;

   MIOP_Error : exception;

   type UIPMC_Profile_Type is new GIOP_Profile_Type with private;
   type UIPMC_Profile_Factory is new GIOP_Profile_Factory with private;

   overriding function Create_Profile
     (PF  : access UIPMC_Profile_Factory;
      Oid :        Objects.Object_Id) return Profile_Access;

   overriding function Duplicate_Profile
     (P : UIPMC_Profile_Type) return Profile_Access;

   overriding function Get_Profile_Tag
     (Profile : UIPMC_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   overriding function Get_Profile_Preference
     (Profile : UIPMC_Profile_Type) return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   overriding procedure Create_Factory
     (PF  : out UIPMC_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access);

   procedure Marshall_UIPMC_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access);

   function Unmarshall_UIPMC_Profile_Body
     (Buffer   : access Buffer_Type) return  Profile_Access;

   overriding function Image (Prof : UIPMC_Profile_Type) return String;

   overriding function Get_OA
     (Profile : UIPMC_Profile_Type) return PolyORB.Smart_Pointers.Entity_Ptr;
   pragma Inline (Get_OA);

private

   --  UIPMC version

   UIPMC_Version_Major : constant Types.Octet := 1;
   UIPMC_Version_Minor : constant Types.Octet := 0;

   type UIPMC_Profile_Type is new GIOP_Profile_Type with record
      G_I : PolyORB.MIOP_P.Groups.Group_Info_Access;
   end record;

   type UIPMC_Profile_Factory is new GIOP_Profile_Factory with null record;

end PolyORB.Binding_Data.GIOP.UIPMC;
