------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . B I N D I N G _ D A T A                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id$

with Ada.Tags;
with Ada.Unchecked_Deallocation;

with PolyORB.Log;

package body PolyORB.Binding_Data is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_data");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Destroy_Profile (P : in out Profile_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Profile_Type'Class, Profile_Access);
   begin
      pragma Assert (P /= null);

      pragma Debug
        (O ("Destroying profile of type "
            & Ada.Tags.External_Tag (P'Tag)));

      Free (P);
   end Destroy_Profile;

   function Get_Object_Key (Profile : Profile_Type)
     return Objects.Object_Id_Access is
   begin
      return Profile.Object_Id;
   end Get_Object_Key;

   procedure Set_Continuation
     (Prof         : access Profile_Type;
      Continuation :        PolyORB.Smart_Pointers.Ref) is
   begin
      pragma Assert (Smart_Pointers.Is_Nil (Prof.Continuation));
      Prof.Continuation := Continuation;
   end Set_Continuation;

end PolyORB.Binding_Data;
