------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . P R O F I L E S . T C                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;

with Broca.Sequences;
with Broca.CDR; use Broca.CDR;
with Broca.Exceptions;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Profiles.TC is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.profiles.tc");
   procedure O is new Broca.Debug.Output (Flag);

   ---------------------
   -- Find_Connection --
   ---------------------

   function Find_Connection
     (Profile : access Multiple_Component_Profile_Type)
     return Connection_Ptr is
   begin
      Broca.Exceptions.Raise_Internal;
      return null;
   end Find_Connection;

   ------------------
   -- Finalization --
   ------------------

   procedure Finalization
     (Profile : in out Multiple_Component_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component_Type, Tagged_Component_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component_Array, Tagged_Components_Ptr);
   begin
      for I in Profile.Components'Range loop
         Free (Profile.Components (I));
      end loop;
      Free (Profile.Components);
   end Finalization;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Multiple_Component_Profile_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      Broca.Exceptions.Raise_Internal;
      return Broca.Sequences.Null_Sequence;
   end Get_Object_Key;

   --------------------------
   -- Get_Profile_Priority --
   --------------------------

   function Get_Profile_Priority
     (Profile : in Multiple_Component_Profile_Type)
     return Profile_Priority is
   begin
      return Profile_Priority'First;
   end Get_Profile_Priority;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : Multiple_Component_Profile_Type)
      return Profile_Tag is
   begin
      return Tag_Multiple_Components;
   end Get_Profile_Tag;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Tagged_Component : Tagged_Component_Type)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Tagged_Component.Tag));
      Marshall (Buffer, Tagged_Component.Component_Data);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Components : in Tagged_Component_Array)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Components'Length));
      for I in Components'Range loop
         pragma Assert (Components (I) /= null);
         Marshall (Buffer, Components (I).all);
      end loop;
   end Marshall;

   ---------------------------
   -- Marshall_Profile_Body --
   ---------------------------

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Multiple_Component_Profile_Type) is
   begin
      pragma Assert (Profile.Components /= null);
      Marshall (Buffer, Profile.Components.all);
   end Marshall_Profile_Body;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
      return Tagged_Component_Access
   is
      Tag  : constant CORBA.Unsigned_Long
        := Unmarshall (Buffer);
      Data : constant Octet_Array
        := Unmarshall (Buffer);
   begin
      return new Tagged_Component_Type'
        (Encapsulation_Size => Data'Length,
         Tag                => Component_Id (Tag),
         Component_Data     => Data);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Tagged_Components_Ptr
   is
      use CORBA;
      Length : CORBA.Unsigned_Long;
      Result : Tagged_Components_Ptr;
   begin
      Length := Unmarshall (Buffer);
      pragma Debug
        (O ("Unmarshalling" & Length'Img & " tagged components."));

      if Length > 0 then
         Result := new Tagged_Component_Array (0 .. Length - 1);
         pragma Assert (Result /= null);

         for I in Result'Range loop
            Result (I) := Unmarshall (Buffer);
         end loop;
      end if;
      return Result;
   end Unmarshall;

end Broca.Profiles.TC;
