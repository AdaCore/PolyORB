------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . I O R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2017, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.References.IOR is

   use Ada.Streams;

   use PolyORB.Binding_Data;
   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Utils;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.ior");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   IOR_Prefix : constant String := "IOR:";

   type Profile_Record is record
      Tag                     : Binding_Data.Profile_Tag;
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type;
   end record;

   package Profile_Record_List is
      new PolyORB.Utils.Chained_Lists (Profile_Record);
   use Profile_Record_List;

   Callbacks : Profile_Record_List.List;

   type IOR_Streamer is new Ref_Streamer with null record;

   overriding procedure Read
     (R : access IOR_Streamer;
      S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Ref'Class);

   overriding procedure Write
     (R : access IOR_Streamer;
      S : access Ada.Streams.Root_Stream_Type'Class;
      V : Ref'Class);

   ----------------------
   -- Marshall_Profile --
   ----------------------

   procedure Marshall_Profile
     (Buffer  : access Buffer_Type;
      P       : Binding_Data.Profile_Access;
      Success : out Boolean)
   is
      use PolyORB.Types;

      T : Profile_Tag;

      Iter : Iterator := First (Callbacks);
   begin
      pragma Assert (P /= null);
      pragma Debug (C, O ("Marshall profile with tag :"
                       & Profile_Tag'Image (Get_Profile_Tag (P.all))));

      Success := False;
      T := Get_Profile_Tag (P.all);

      while not Last (Iter) loop
         declare
            Info : constant Profile_Record := Value (Iter).all;
         begin
            pragma Debug
              (C, O ("... with callback whose tag is "
                  & Profile_Tag'Image (Info.Tag)));

            if T = Info.Tag then
               Marshall (Buffer, Types.Unsigned_Long (T));
               Value (Iter).Marshall_Profile_Body (Buffer, P);
               Success := True;

               return;
            end if;
         end;
         Next (Iter);
      end loop;
   end Marshall_Profile;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (R : access IOR_Streamer;
      S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Ref'Class)
   is
      pragma Unreferenced (R);
      Opaque : aliased Stream_Element_Array := Stream_Element_Array'Input (S);
   begin
      Ref (V) := Opaque_To_Object (Opaque'Unchecked_Access);
   end Read;

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

      Iter : Iterator := First (Callbacks);
   begin
      pragma Debug (C, O ("Considering profile with tag"
                       & Profile_Tag'Image (Tag)));

      while not Last (Iter) loop
         pragma Debug
           (C, O ("... with callback whose tag is "
               & Profile_Tag'Image (Value (Iter).Tag)));

         if Value (Iter).Tag = Tag then
            Prof := Value (Iter).Unmarshall_Profile_Body (Buffer);
            Known := True;
            --  Profiles dynamically allocated here
            --  will be freed when the returned
            --  reference is finalised.
         end if;
         Next (Iter);
      end loop;

      if not Known then
         --  No callback matches this tag.
         declare
            pragma Debug (C, O ("Profile with tag"
                             & Profile_Tag'Image (Tag)
                             & " not found"));

            pragma Warnings (Off);
            Discarded_Body : constant Encapsulation := Unmarshall (Buffer);
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
      Value  : PolyORB.References.Ref)
   is
      use PolyORB.Types;

   begin
      pragma Debug (C, O ("Marshall IOR: Enter"));

      if Is_Nil (Value) then
         Marshall
           (Buffer,
            PolyORB.Types.RepositoryId'(To_PolyORB_String ("")));
         Marshall (Buffer, Types.Unsigned_Long'(0));
         pragma Debug (C, O ("Empty IOR"));

      else
         Marshall
           (Buffer,
            PolyORB.Types.RepositoryId'
              (To_PolyORB_String (Type_Id_Of (Value))));

         Pad_Align (Buffer, Align_4);

         declare
            Profs     : constant Profile_Array := Profiles_Of (Value);
            Counter   : Types.Unsigned_Long := 0;
            Count_Buf : Buffer_Access := new Buffer_Type;
            Reserv    : Reservation;
            Success   : Boolean;
         begin
            Set_Initial_Position (Count_Buf, CDR_Position (Buffer));
            Reserv := Reserve (Buffer, Counter'Size / Types.Octet'Size);
            pragma Debug (C, O (Type_Id_Of (Value)));

            for Profile_Index in Profs'Range loop
               pragma Assert (Profs (Profile_Index) /= null);
               Marshall_Profile (Buffer, Profs (Profile_Index), Success);

               if Success then
                  Counter := Counter + 1;
               else
                  pragma Debug (C, O ("Profile with tag"
                                   & Profile_Tag'Image
                                   (Get_Profile_Tag
                                    (Profs (Profile_Index).all))
                                   & " not found"));
                  null;
               end if;
            end loop;

            pragma Debug (C, O (Types.Unsigned_Long'Image (Counter)
                             & " profile(s)"));

            Marshall (Count_Buf, Counter);
            Copy_Data (Count_Buf.all, Reserv);
            Release (Count_Buf);
         end;
      end if;
      pragma Debug (C, O ("Marshall IOR: Leave"));
   end Marshall_IOR;

   --------------------
   -- Unmarshall_IOR --
   --------------------

   function Unmarshall_IOR
     (Buffer : access Buffer_Type)
     return  PolyORB.References.Ref
   is
      use PolyORB.Types;

      Result     : PolyORB.References.Ref;

      PolyORB_Type_Id : constant Types.String
        := Types.String (Types.RepositoryId'(Unmarshall (Buffer)));
      Type_Id : constant String
        := To_Standard_String (PolyORB_Type_Id);

      N_Profiles : constant Types.Unsigned_Long
        := Unmarshall (Buffer);

      Profs   : Profile_Array := (1 .. Integer (N_Profiles) => null);
      Last_Profile : Integer := Profs'First - 1;
   begin

      pragma Debug
        (C, O ("Decapsulate_IOR: type " & Type_Id
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

   function Object_To_Opaque (IOR : PolyORB.References.Ref)
     return Stream_Element_Array
   is
      Buf : Buffer_Access := new Buffer_Type;
   begin
      Start_Encapsulation (Buf);
      Marshall (Buf, IOR);

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
     return PolyORB.References.Ref
   is
      Buf : aliased Buffer_Type;
   begin
      Decapsulate (Opaque, Buf'Access);
      return Unmarshall (Buf'Access);
   end Opaque_To_Object;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String (IOR : PolyORB.References.Ref) return String
   is
   begin
      return IOR_Prefix & SEA_To_Hex_String (Object_To_Opaque (IOR));
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object (Str : String) return PolyORB.References.Ref
   is
   begin
      pragma Debug (C, O ("Try to decode IOR"));
      if Has_Prefix (Str, IOR_Prefix) then
         pragma Debug (C, O ("IOR Header ok"));
         declare
            Octets : aliased Stream_Element_Array
              := Hex_String_To_SEA
              (Str (Str'First + IOR_Prefix'Length .. Str'Last));
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
     (Profile     : Profile_Tag;
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type)
   is
      Elt : constant Profile_Record :=
        (Profile, Marshall_Profile_Body, Unmarshall_Profile_Body);
   begin
      Append (Callbacks, Elt);
   end Register;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (R : access IOR_Streamer;
      S : access Ada.Streams.Root_Stream_Type'Class;
      V : Ref'Class)
   is
      pragma Unreferenced (R);
   begin
      Stream_Element_Array'Output (S, Object_To_Opaque (Ref (V)));
   end Write;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_String_To_Object (IOR_Prefix, String_To_Object'Access);
      References.The_Ref_Streamer := new IOR_Streamer;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"references.ior",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => +"references",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.References.IOR;
