------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E F E R E N C E S . C O R B A L O C           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with PolyORB.Binding_Data;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.References.Corbaloc is

   use PolyORB.Binding_Data;
   use PolyORB.Log;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.corbaloc");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Profile_Record is record
      Tag                    : PolyORB.Binding_Data.Profile_Tag;
      Proto_Ident            : Types.String;
      Profile_To_String_Body : Profile_To_String_Body_Type;
      String_To_Profile_Body : String_To_Profile_Body_Type;
   end record;

   function "=" (A, B : Profile_Record) return Boolean;

   function "=" (A, B : Profile_Record) return Boolean is
      use PolyORB.Types;

   begin
      return A.Tag = B.Tag;
   end "=";

   package Profile_Record_List is
      new PolyORB.Utils.Chained_Lists (Profile_Record, "=");

   use Profile_Record_List;

   Callbacks : Profile_Record_List.List;

   Null_String : constant Types.String
     := PolyORB.Types.To_PolyORB_String ("");

   type Tag_Array is array (Natural range <>) of Profile_Tag;

   -----------------------
   -- Get_Corbaloc_List --
   -----------------------

   procedure Get_Corbaloc_List
     (Corbaloc      :     Corbaloc_Type;
      Corbaloc_List : out String_Array;
      Tag_List      : out Tag_Array;
      N             : out Natural);
   --  Return the list of all corbaloc found in Corbaloc

   procedure Get_Corbaloc_List
     (Corbaloc      :     Corbaloc_Type;
      Corbaloc_List : out String_Array;
      Tag_List      : out Tag_Array;
      N             : out Natural)
   is
      use PolyORB.Types;

      Profs : constant Profile_Array := Profiles_Of (Corbaloc);
      Str : Types.String;
   begin
      N := 0;

      for J in Profs'Range loop
         Str := Profile_To_String (Profs (J));
         if Length (Str) /= 0 then
            N := N + 1;
            Corbaloc_List (N) := Str;
            Tag_List (N) := Get_Profile_Tag (Profs (J).all);
         end if;
      end loop;

      pragma Debug (O ("Profile found :" & Natural'Image (N)));
   end Get_Corbaloc_List;

   -----------------------
   -- Profile_To_String --
   -----------------------

   function Profile_To_String
     (P : Binding_Data.Profile_Access)
     return Types.String
   is
      use PolyORB.Types;
      use Profile_Record_List;

      T    : Profile_Tag;
      Iter : Iterator := First (Callbacks);
   begin
      pragma Assert (P /= null);
      pragma Debug (O ("Profile to string with tag:"
                       & Profile_Tag'Image (Get_Profile_Tag (P.all))));

      T := Get_Profile_Tag (P.all);

      while Iter /= Last (Callbacks) loop
         declare
            Info : constant Profile_Record := Value (Iter).all;
         begin
            if T = Info.Tag then
               declare
                  Str : constant Types.String
                    := Info.Profile_To_String_Body (P);
               begin
                  if Length (Str) /= 0 then
                     pragma Debug (O ("Profile ok"));
                     return Str;
                  else
                     pragma Debug (O ("Profile not ok"));
                     return Null_String;
                  end if;
               end;
            end if;
         end;
         Next (Iter);
      end loop;

      pragma Debug (O ("Profile not ok"));
      return Null_String;
   end Profile_To_String;

   -----------------------
   -- String_To_Profile --
   -----------------------

   function String_To_Profile
     (Str : Types.String)
     return Binding_Data.Profile_Access
   is
      use PolyORB.Types;
      use Profile_Record_List;

      Iter : Iterator := First (Callbacks);
   begin
      pragma Debug (O ("String_To_Profile: enter with "
                       & To_Standard_String (Str)));

      while Iter /= Last (Callbacks) loop
         declare
            Ident : Types.String renames Value (Iter).Proto_Ident;
         begin
            if Length (Str) > Length (Ident)
              and then To_String (Str) (1 .. Length (Ident)) = Ident then
               pragma Debug
                 (O ("Try to unmarshall profile with profile factory tag "
                     & Profile_Tag'Image (Value (Iter).Tag)));
               return Value (Iter).String_To_Profile_Body (Str);
            end if;
         end;
         Next (Iter);
      end loop;

      pragma Debug (O ("Profile not found for : "
                       & To_Standard_String (Str)));
      return null;
   end String_To_Profile;

   ----------------------------------------
   -- Object_To_String_With_Best_Profile --
   ----------------------------------------

   function Object_To_String_With_Best_Profile
     (Corbaloc : Corbaloc_Type)
     return Types.String is
   begin
      pragma Debug (O ("Create corbaloc with best profile: Enter"));

      if Is_Nil (Corbaloc) then
         pragma Debug (O ("Corbaloc Empty"));
         return Types.To_PolyORB_String (Corbaloc_Prefix);
      else
         declare
            use PolyORB.Types;

            N : Natural;
            TL : Tag_Array (1 .. Length (Callbacks));
            SL : String_Array (1 .. Length (Callbacks));
            Profs : constant Profile_Array := Profiles_Of (Corbaloc);
            Best_Preference : Profile_Preference := Profile_Preference'First;
            Best_Profile_Index : Integer := 0;
            Str : Types.String := To_PolyORB_String (Corbaloc_Prefix);
         begin
            Get_Corbaloc_List (Corbaloc, SL, TL, N);

            for J in Profs'Range loop
               declare
                  P : constant Profile_Preference
                    := Get_Profile_Preference (Profs (J).all);
               begin
                  if P > Best_Preference then
                     for K in 1 .. N loop
                        if TL (K) = Get_Profile_Tag (Profs (J).all) then
                           Best_Preference := P;
                           Best_Profile_Index := K;
                        end if;
                     end loop;
                  end if;
               end;
            end loop;

            if Best_Profile_Index > 0 then
               Append (Str, SL (Best_Profile_Index));
            end if;

            pragma Debug (O ("Create corbaloc with best profile: Leave"));
            return Str;
         end;
      end if;
   end Object_To_String_With_Best_Profile;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Corbaloc : Corbaloc_Type;
      Profile  : PolyORB.Binding_Data.Profile_Tag)
     return Types.String
   is
      use PolyORB.Types;

      Profs : constant Profile_Array := Profiles_Of (Corbaloc);
      Str : Types.String;
   begin
      for J in Profs'Range loop
         if Get_Profile_Tag (Profs (J).all) = Profile then
            Str := Profile_To_String (Profs (J));
            if Length (Str) /= 0 then
               return Str;
            end if;
         end if;
      end loop;
      return Types.To_PolyORB_String (Corbaloc_Prefix);
   end Object_To_String;

   -----------------------
   -- Object_To_Strings --
   -----------------------

   function Object_To_Strings (Corbaloc : Corbaloc_Type) return String_Array
   is
      N : Natural;
      TL : Tag_Array (1 .. Length (Callbacks));
      SL : String_Array (1 .. Length (Callbacks));
   begin
      Get_Corbaloc_List (Corbaloc, SL, TL, N);
      return SL (1 .. N);
   end Object_To_Strings;

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object (Str : String) return Corbaloc_Type
   is
      use PolyORB.Types;
      use Profile_Seqs;

      Result : Corbaloc_Type;
      Pro    : Profile_Access;
   begin
      pragma Debug (O ("Try to decode Corbaloc: enter "));
      if Utils.Has_Prefix (Str, Corbaloc_Prefix) then
         Pro := String_To_Profile
           (To_PolyORB_String
            (Str (Corbaloc_Prefix'Length + 1 .. Str'Last)));
         if Pro /= null then
            Create_Reference ((1 => Pro), "", References.Ref (Result));
         end if;
      end if;

      pragma Debug (O ("Try to decode Corbaloc: leave "));
      return Result;
   end String_To_Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag                    : in PolyORB.Binding_Data.Profile_Tag;
      Proto_Ident            : in Types.String;
      Profile_To_String_Body : in Profile_To_String_Body_Type;
      String_To_Profile_Body : in String_To_Profile_Body_Type)
   is
      Elt : constant Profile_Record
        := (Tag,
            Proto_Ident,
            Profile_To_String_Body,
            String_To_Profile_Body);
   begin
      Append (Callbacks, Elt);
   end Register;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_String_To_Object
        (Corbaloc_Prefix, String_To_Object'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"references.corbaloc",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => +"references",
       Init      => Initialize'Access));
end PolyORB.References.Corbaloc;
