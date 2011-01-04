------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . B I N D I N G _ D A T A                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2011, Free Software Foundation, Inc.          --
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

--  Management of binding data, i. e. the elements of information that denote
--  the association of a middleware TSAP address, a protocol, and an object id,
--  together constituting a profile.

with Ada.Tags;
with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Setup;

package body PolyORB.Binding_Data is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_data");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ---------------------
   -- Destroy_Profile --
   ---------------------

   procedure Destroy_Profile (P : in out Profile_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Profile_Type'Class, Profile_Access);
   begin
      pragma Assert (P /= null);

      pragma Debug
        (C, O ("Destroying profile of type "
            & Ada.Tags.External_Tag (P'Tag)));

      Release (P.all);
      Free (P);
   end Destroy_Profile;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : Profile_Type) return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);
   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   end Get_OA;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key (Profile : Profile_Type)
     return Objects.Object_Id_Access is
   begin
      return Profile.Object_Id;
   end Get_Object_Key;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile (P : Profile_Type'Class) return Boolean is
   begin
      return P.Known_Local;
   end Is_Local_Profile;

   --------------------------
   -- Is_Multicast_Profile --
   --------------------------

   function Is_Multicast_Profile (P : Profile_Type) return Boolean is
      pragma Unreferenced (P);
   begin
      return False;
   end Is_Multicast_Profile;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (Prof : access Profile_Type)
      return Annotations.Notepad_Access
   is
   begin
      return Prof.Notepad'Access;
   end Notepad_Of;

   ---------------
   -- Same_Node --
   ---------------

   function Same_Node (Left : Profile_Type'Class; Right : Profile_Type'Class)
     return Boolean
   is
   begin

      --  Is_Colocated depends on the order of the arguments.
      --  Imagine we want to compare a Neighbour profile (N) and a
      --  SOAP profile (S). N binds to a binding object with a profile on the
      --  same node than S.
      --  Is_Colocated (S, N) will return False because the derived
      --  Is_Colocated for SOAP profiles does not know how to manage neighbour
      --  profiles. But Is_Colocated (N, S) will return True as the derived
      --  Is_Colocated for Neighbour profiles does extract the true profile of
      --  N from its Binding_Object.

      --  Same_Node takes into account both the derived Is_Colocated for Left
      --  and Right. Same_Node is therefore a symmetric predicate.

      return Is_Colocated (Left => Left, Right => Right)
        or else Is_Colocated (Left => Right, Right => Left);
   end Same_Node;

   ---------------------
   -- Same_Object_Key --
   ---------------------

   function Same_Object_Key (Left, Right : Profile_Type'Class) return Boolean
   is
      use PolyORB.Objects;
   begin
      if Left.Object_Id = null or else Right.Object_Id = null then
         return False;
      else
         return Left.Object_Id.all = Right.Object_Id.all;
      end if;
   end Same_Object_Key;

   ----------------------
   -- Set_Continuation --
   ----------------------

   procedure Set_Continuation
     (Prof         : access Profile_Type;
      Continuation :        PolyORB.Smart_Pointers.Ref) is
   begin
      pragma Assert (Smart_Pointers.Is_Nil (Prof.Continuation));
      Prof.Continuation := Continuation;
   end Set_Continuation;

end PolyORB.Binding_Data;
