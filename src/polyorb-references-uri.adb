------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . U R I                --
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

package body PolyORB.References.URI is

   use PolyORB.Binding_Data;
   use PolyORB.Log;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.uri");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Profile_Record is record
      Tag                    : PolyORB.Binding_Data.Profile_Tag;
      Proto_Ident            : Types.String;
      Profile_To_String_Body : Profile_To_String_Body_Type;
      String_To_Profile_Body : String_To_Profile_Body_Type;
   end record;

   package Profile_Record_List is
      new PolyORB.Utils.Chained_Lists (Profile_Record);
   use Profile_Record_List;

   Callbacks : Profile_Record_List.List;

   Empty_String : constant String := "";

   type Tag_Array is array (Natural range <>) of Profile_Tag;

   ----------
   -- Free --
   ----------

   procedure Free (SA : in out String_Array)
   is
   begin
      for J in SA'Range loop
         Free (SA (J));
      end loop;
   end Free;

   ------------------
   -- Get_URI_List --
   ------------------

   procedure Get_URI_List
     (URI      :     URI_Type;
      URI_List : out String_Array;
      Tag_List : out Tag_Array;
      N        : out Natural);
   --  Return the list of all URIs found in URI

   procedure Get_URI_List
     (URI      :     URI_Type;
      URI_List : out String_Array;
      Tag_List : out Tag_Array;
      N        : out Natural)
   is
      Profs : constant Profile_Array := Profiles_Of (URI);
   begin
      N := 0;

      for J in Profs'Range loop
         declare
            Str : constant String := Profile_To_String (Profs (J));
         begin
            if Str'Length /= 0 then
               N := N + 1;
               URI_List (N) := new String'(Str);
               Tag_List (N) := Get_Profile_Tag (Profs (J).all);
            end if;
         end;
      end loop;

      pragma Debug (O ("Profile found :" & Natural'Image (N)));
   end Get_URI_List;

   -----------------------
   -- Profile_To_String --
   -----------------------

   function Profile_To_String
     (P : Binding_Data.Profile_Access)
     return String
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

      while not Last (Iter) loop
         declare
            Info : constant Profile_Record := Value (Iter).all;
         begin
            if T = Info.Tag then
               declare
                  Str : constant String :=
                    To_String (Info.Profile_To_String_Body (P));
               begin
                  if Str'Length /= 0 then
                     pragma Debug (O ("Profile ok"));
                     return Str;
                  else
                     pragma Debug (O ("Profile not ok"));
                     return Empty_String;
                  end if;
               end;
            end if;
         end;

         Next (Iter);
      end loop;

      pragma Debug (O ("Profile not ok"));
      return Empty_String;
   end Profile_To_String;

   -----------------------
   -- String_To_Profile --
   -----------------------

   function String_To_Profile
     (Str : String)
     return Binding_Data.Profile_Access
   is
      use PolyORB.Types;
      use Profile_Record_List;

      Iter : Iterator := First (Callbacks);
   begin
      pragma Debug (O ("String_To_Profile: enter with "
                       & Str));

      while not Last (Iter) loop
         declare
            Ident : String renames To_String (Value (Iter).Proto_Ident);
         begin
            if Str'Length > Ident'Length
              and then Str (Ident'Range) = Ident then
               pragma Debug
                 (O ("Try to unmarshall profile with profile factory tag "
                     & Profile_Tag'Image (Value (Iter).Tag)));
               return Value (Iter).String_To_Profile_Body
                 (To_PolyORB_String (Str));
            end if;
         end;

         Next (Iter);
      end loop;

      pragma Debug (O ("Profile not found for : " & Str));
      return null;
   end String_To_Profile;

   ----------------------------------------
   -- Object_To_String_With_Best_Profile --
   ----------------------------------------

   function Object_To_String_With_Best_Profile
     (URI : URI_Type)
     return String
   is
   begin
      pragma Debug (O ("Create URI with best profile: Enter"));

      if Is_Nil (URI) then
         pragma Debug (O ("URI is Empty"));
         return Empty_String;
      else
         declare
            use PolyORB.Types;

            N  : Natural;
            TL : Tag_Array (1 .. Length (Callbacks));
            SL : String_Array (1 .. Length (Callbacks));
            Profs : constant Profile_Array := Profiles_Of (URI);
            Best_Preference : Profile_Preference := Profile_Preference'First;
            Best_Profile_Index : Integer := 0;
         begin
            Get_URI_List (URI, SL, TL, N);

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

            pragma Debug (O ("Create URI with best profile: Leave"));

            if Best_Profile_Index > 0 then
               declare
                  Str : constant String := SL (Best_Profile_Index).all;
               begin
                  Free (SL);
                  return Str;
               end;
            else
               return Empty_String;
            end if;
         end;
      end if;
   end Object_To_String_With_Best_Profile;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (URI     : URI_Type;
      Profile : PolyORB.Binding_Data.Profile_Tag)
     return String
   is
      use PolyORB.Types;

      Profs : constant Profile_Array := Profiles_Of (URI);
   begin
      for J in Profs'Range loop
         if Get_Profile_Tag (Profs (J).all) = Profile then
            declare
               Str : constant String := (Profile_To_String (Profs (J)));
            begin
               if Str'Length /= 0 then
                  return Str;
               end if;
            end;
         end if;
      end loop;
      return Empty_String;
   end Object_To_String;

   -----------------------
   -- Object_To_Strings --
   -----------------------

   function Object_To_Strings (URI : URI_Type) return String_Array
   is
      N : Natural;
      TL : Tag_Array (1 .. Length (Callbacks));
      SL : String_Array (1 .. Length (Callbacks));
   begin
      Get_URI_List (URI, SL, TL, N);
      return SL (1 .. N);
   end Object_To_Strings;

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object (Str : String) return URI_Type
   is
      use PolyORB.Types;
      use Profile_Seqs;

      Result : URI_Type;
      Pro    : Profile_Access;
   begin
      pragma Debug (O ("Try to decode URI: enter "));
      Pro := String_To_Profile (Str);

      if Pro /= null then
         Create_Reference ((1 => Pro), "", References.Ref (Result));
      end if;

      pragma Debug (O ("Try to decode URI: leave "));
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
      use PolyORB.Types;
      use Profile_Record_List;

      Iter : Iterator := First (Callbacks);
   begin
      while not Last (Iter) loop
         Register_String_To_Object
           (PolyORB.Types.To_String (Value (Iter).Proto_Ident),
            String_To_Object'Access);
         Next (Iter);
      end loop;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"references.uri",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => +"binding_factories",
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.References.URI;
