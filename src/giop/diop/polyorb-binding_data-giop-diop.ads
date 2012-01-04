------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . D I O P        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Binding data concrete implementation for DIOP

with PolyORB.Buffers;
with PolyORB.Types;

package PolyORB.Binding_Data.GIOP.DIOP is

   use PolyORB.Buffers;

   type DIOP_Profile_Type is new GIOP_Profile_Type with private;
   type DIOP_Profile_Factory is new GIOP_Profile_Factory with private;

   function Create_Profile
     (PF  : access DIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access;

   function Duplicate_Profile
     (P : DIOP_Profile_Type)
     return Profile_Access;

   function Get_Profile_Tag (Profile : DIOP_Profile_Type) return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : DIOP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   procedure Create_Factory
     (PF  : out DIOP_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access);

   procedure Marshall_DIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access);

   function Unmarshall_DIOP_Profile_Body
     (Buffer : access Buffer_Type)
    return Profile_Access;

   function Image (Prof : DIOP_Profile_Type) return String;

private

   --  DIOP version

   DIOP_Version_Major : constant Types.Octet := 1;
   DIOP_Version_Minor : constant Types.Octet := 0;

   type DIOP_Profile_Type is new GIOP_Profile_Type with null record;

   type DIOP_Profile_Factory is new GIOP_Profile_Factory with null record;

end PolyORB.Binding_Data.GIOP.DIOP;
