------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . I O R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  $Id$

with Ada.Streams;

with PolyORB.Binding_Data;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Log;
with PolyORB.Representations.CDR;
with PolyORB.Sequences.Unbounded;
with PolyORB.Types;
--  with PolyORB.Utils.Strings;

package body PolyORB.References.IOR is

   use Ada.Streams;

   use PolyORB.Binding_Data;
   use PolyORB.Log;
   use PolyORB.Representations.CDR;
   use PolyORB.Utils;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.ior");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   IOR_Prefix : constant String := "IOR:";

   type Profile_Record is record
      Tag                     : Binding_Data.Profile_Tag;
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type;
   end record;

   package Profile_Record_Seq is
      new PolyORB.Sequences.Unbounded (Profile_Record);

   use Profile_Record_Seq;

   Callbacks : Profile_Record_Seq.Sequence;

   ----------------------
   -- Marshall Profile --
   ----------------------

   procedure Marshall_Profile
     (Buffer  : access Buffer_Type;
      P       :        Binding_Data.Profile_Access;
      Success :    out Boolean)
   is
      use PolyORB.Types;
   begin
      Success := False;
      pragma Assert (P /= null);
      pragma Debug (O ("Marshall profile with tag :"
                       & Get_Profile_Tag (P.all)'Img));
      for J in 1 .. Length (Callbacks) loop
         declare
            T : constant Profile_Tag
              := Get_Profile_Tag (P.all);

            Info : constant Profile_Record
              := Element_Of (Callbacks, J);
         begin
            pragma Debug
              (O ("... with callback" & Integer'Image (J)
                  & " whose tag is "
                  & Profile_Tag'Image (Info.Tag)));

            if T = Info.Tag then
               Marshall
                 (Buffer, Types.Unsigned_Long
                  (T));
               Element_Of (Callbacks, J).Marshall_Profile_Body
                 (Buffer, P);
               Success := True;
               return;
            end if;
         end;
      end loop;
   end Marshall_Profile;

   ------------------------
   -- Unmarshall_Profile --
   ------------------------

   function Unmarshall_Profile
     (Buffer : access Buffer_Type)
     return Profile_Access
   is
      use PolyORB.Types;

      Temp_Tag : constant Types.Unsigned_Long := Unmarshall (Buffer);
      Tag      : constant Profile_Tag := Profile_Tag (Temp_Tag);
      Known    : Boolean := False;
      Prof     : Profile_Access;
   begin
      pragma Debug (O ("Considering profile with tag" & Tag'Img));

      for J in 1 .. Length (Callbacks) loop
         pragma Debug
           (O ("... with callback" & Integer'Image (J)
               & " whose tag is "
               & Profile_Tag'Image (Element_Of (Callbacks, J).Tag)));

         if Element_Of (Callbacks, J).Tag = Tag then
            Prof := Element_Of (Callbacks, J).Unmarshall_Profile_Body (Buffer);
            Known := True;
            --  Profiles dynamically allocated here
            --  will be freed when the returned
            --  reference is finalised.
         end if;
      end loop;

      if not Known then
         --  No callback matches this tag.
         declare
            pragma Debug (O ("Profile with tag"
                             & Tag'Img
                             & " not found"));
            pragma Warnings (Off);
            Discarded_Body : constant Encapsulation
              := Unmarshall (Buffer);
            --  Consider the profile body as an encapsulation
            --  (our best bet).
            pragma Unreferenced (Discarded_Body);
            pragma Warnings (On);
         begin
            return null;
         end;
      end if;
      return Prof;
   end Unmarshall_Profile;

   ------------------
   -- Marshall_IOR --
   ------------------

   procedure Marshall_IOR
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type)
   is
      use PolyORB.Types;
      use Profile_Seqs;

   begin
      pragma Debug (O ("Marshall IOR: Enter"));

      if Is_Nil (Value) then
         Marshall
           (Buffer, PolyORB.Types.String'(To_PolyORB_String ("")));
         Marshall (Buffer, Types.Unsigned_Long'(0));
         pragma Debug (O ("IOR Empty"));
      else
         Marshall
           (Buffer, PolyORB.Types.String'
            (To_PolyORB_String (Type_Id_Of (Value))));
         Pad_Align (Buffer, 4);
         declare
            Profs    : constant Profile_Array
              := Profiles_Of (Value);
            Counter  : Types.Unsigned_Long := 0;
            Count_Buf : Buffer_Access := new Buffer_Type;
            Reserv   : Reservation;
            Success  : Boolean;
         begin
            Set_Initial_Position (Count_Buf, CDR_Position (Buffer));
            Reserv := Reserve (Buffer, Counter'Size / Types.Octet'Size);
            pragma Debug (O (Type_Id_Of (Value)));
            for Profile_Index in Profs'Range loop
               pragma Assert (Profs (Profile_Index) /= null);
               Marshall_Profile (Buffer, Profs (Profile_Index), Success);
               if Success then
                  Counter := Counter + 1;
               else
                  pragma Debug (O ("Profile with tag"
                                   & Get_Profile_Tag
                                   (Profs (Profile_Index).all)'Img
                                   & " not found"));
                  null;
               end if;
            end loop;

            pragma Debug (O (Counter'Img & " profile(s)"));

            Marshall (Count_Buf, Counter);
            Copy_Data (Count_Buf.all, Reserv);
            Release (Count_Buf);
         end;
      end if;
      pragma Debug (O ("Marshall IOR: Leave"));
   end Marshall_IOR;

   --------------------
   -- Unmarshall_IOR --
   --------------------

   function Unmarshall_IOR
     (Buffer : access Buffer_Type)
     return  IOR_Type
   is
      use PolyORB.Types;
      use Profile_Seqs;

      Result     : IOR_Type;

      PolyORB_Type_Id : constant Types.String
        := Types.String (Types.String'(Unmarshall (Buffer)));
      Type_Id : constant String
        := To_Standard_String (PolyORB_Type_Id);

      N_Profiles : constant Types.Unsigned_Long
        := Unmarshall (Buffer);

      Profs   : Profile_Array := (1 .. Integer (N_Profiles) => null);
      Last_Profile : Integer := Profs'First - 1;
   begin

      pragma Debug
        (O ("Decapsulate_IOR: type " & Type_Id
            & " (" & Unsigned_Long'Image (N_Profiles) & " profiles)."));

      for N in 1 .. N_Profiles loop
         declare
            Pro : Profile_Access;
         begin
            Pro := Unmarshall_Profile (Buffer);
            if Pro /= null then
               Last_Profile := Last_Profile + 1;
               Profs (Last_Profile) := Pro;
            end if;
         end;
      end loop;

      if Last_Profile >= Profs'First then
         Create_Reference
           (Profs (Profs'First .. Last_Profile), Type_Id,
            References.Ref (Result));
      end if;

      return Result;
   end Unmarshall_IOR;

   ----------------------
   -- Object_To_Opaque --
   ----------------------

   function Object_To_Opaque (IOR : IOR_Type)
     return Stream_Element_Array
   is
      Buf : Buffer_Access := new Buffer_Type;
   begin
      Start_Encapsulation (Buf);
      Representations.CDR.Marshall (Buf, IOR);

      declare
         Octets : constant Encapsulation := Encapsulate (Buf);
      begin
         Release (Buf);
         return Stream_Element_Array (Octets);
      end;
   end Object_To_Opaque;

   ----------------------
   -- Opaque_To_Object --
   ----------------------

   function Opaque_To_Object
     (Opaque : access Ada.Streams.Stream_Element_Array)
     return IOR_Type
   is
      Buf : aliased Buffer_Type;
   begin
      Decapsulate (Opaque, Buf'Access);
      return Unmarshall (Buf'Access);
   end Opaque_To_Object;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (IOR : IOR_Type)
     return Types.String
   is
      use PolyORB.Types;
   begin
      return To_PolyORB_String (IOR_Prefix)
        & To_String (Object_To_Opaque (IOR));
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object
     (Str : Types.String)
     return IOR_Type
   is
      use PolyORB.Types;
      use PolyORB.Buffers;
      use PolyORB.Utils.Strings;

      S : constant String := To_Standard_String (Str);

   begin
      pragma Debug (O ("Try to decode IOR"));
      if Has_Prefix (S, IOR_Prefix) then
         pragma Debug (O ("IOR Header ok"));
         declare
            Octets : aliased Stream_Element_Array
              := To_Stream_Element_Array
              (S (S'First + IOR_Prefix'Length .. S'Last));
         begin
            return Opaque_To_Object (Octets'Access);
         end;
      end if;
      raise Constraint_Error;
   end String_To_Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Profile     : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type)
   is
      Elt : constant Profile_Record
        := (Profile, Marshall_Profile_Body,
            Unmarshall_Profile_Body);
   begin
      Append (Callbacks, Elt);
   end Register;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register (Types.To_PolyORB_String (IOR_Prefix), String_To_Object'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"references.ior",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Init      => Initialize'Access));
end PolyORB.References.IOR;
