------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ D A T A . L O C A L            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Contact information for an object that exists
--  within the local ORB.

--  $Id$

with PolyORB.Objects;

package PolyORB.Binding_Data.Local is

   pragma Elaborate_Body;

   type Local_Profile_Type is new Profile_Type with private;

   procedure Initialize (P : in out Local_Profile_Type);
   procedure Adjust (P : in out Local_Profile_Type);
   procedure Finalize (P : in out Local_Profile_Type);

   procedure Create_Local_Profile
     (Oid : Objects.Object_Id;
      P   : out Local_Profile_Type);

   function Bind_Profile
     (Profile : Local_Profile_Type;
      The_ORB : Components.Component_Access)
     return Components.Component_Access;

   function Get_Profile_Tag
     (Profile : Local_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : Local_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   function Image (Prof : Local_Profile_Type) return String;

   function Get_OA
     (Profile : Local_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr;
   pragma Inline (Get_OA);

   --  Since Local profiles are not associated with any
   --  transport endpoint, there is no need to define
   --  an associated Profile_Factory.

private

   type Local_Profile_Type is new Profile_Type with null record;

end PolyORB.Binding_Data.Local;
