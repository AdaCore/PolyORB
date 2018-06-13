------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ D A T A . L O C A L            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Profile type for objects registered with the local ORB

with PolyORB.Objects;

package PolyORB.Binding_Data.Local is

   pragma Elaborate_Body;

   type Local_Profile_Type is new Profile_Type with private;

   overriding procedure Release (P : in out Local_Profile_Type);

   procedure Create_Local_Profile
     (Oid : Objects.Object_Id;
      P   : out Local_Profile_Type);

   overriding function Duplicate_Profile
     (P : Local_Profile_Type)
     return Profile_Access;

   overriding procedure Bind_Profile
     (Profile : access Local_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container);

   overriding function Get_Profile_Tag
     (Profile : Local_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   overriding function Get_Profile_Preference
     (Profile : Local_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   overriding function Image (Prof : Local_Profile_Type) return String;

   overriding function Is_Colocated
     (Left  : Local_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

   --  Since Local profiles are not associated with any
   --  transport endpoint, there is no need to define
   --  an associated Profile_Factory.

private

   type Local_Profile_Type is new Profile_Type with null record;

end PolyORB.Binding_Data.Local;
