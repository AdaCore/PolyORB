------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . G I O P . U I P M C       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

--  Binding data concrete implementation for UIPMC

with PolyORB.Buffers;
with PolyORB.MIOP_P.Groups;
with PolyORB.Types;

package PolyORB.Binding_Data.GIOP.UIPMC is

   use PolyORB.Buffers;

   MIOP_Error : exception;

   type UIPMC_Profile_Type is new GIOP_Profile_Type with private;
   type UIPMC_Profile_Factory is new GIOP_Profile_Factory with private;

   function Create_Profile
     (PF  : access UIPMC_Profile_Factory;
      Oid :        Objects.Object_Id) return Profile_Access;

   function Duplicate_Profile
     (P : UIPMC_Profile_Type) return Profile_Access;

   function Get_Profile_Tag (Profile : UIPMC_Profile_Type) return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : UIPMC_Profile_Type) return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   procedure Create_Factory
     (PF  : out UIPMC_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access);

   procedure Marshall_UIPMC_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access);

   function Unmarshall_UIPMC_Profile_Body
     (Buffer   : access Buffer_Type) return  Profile_Access;

   function Image (Prof : UIPMC_Profile_Type) return String;

   function Get_OA
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
