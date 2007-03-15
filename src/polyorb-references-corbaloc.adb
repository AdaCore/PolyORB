------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E F E R E N C E S . C O R B A L O C           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Types;

package body PolyORB.References.Corbaloc is

   use PolyORB.Binding_Data;
   use PolyORB.Log;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.corbaloc");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   Corbaloc_Prefix : constant String := "corbaloc:";

   --  A descriptor is kept for each profile kind that is supported within
   --  the corbaloc: scheme.

   type Profile_Record is record
      Tag                    : PolyORB.Binding_Data.Profile_Tag;
      --  Profile tag

      Proto_Ident            : String_Ptr;
      --  Protocol token

      Profile_To_String_Body : Profile_To_String_Body_Type;
      String_To_Profile_Body : String_To_Profile_Body_Type;
      --  <protocol>_addr <-> profile conversion functions

   end record;

   package Profile_Record_List is
      new PolyORB.Utils.Chained_Lists (Profile_Record);
   use Profile_Record_List;

   Callbacks : Profile_Record_List.List;

   Null_String : constant Types.String := PolyORB.Types.To_PolyORB_String ("");

   type Tag_Array is array (Natural range <>) of Profile_Tag;

   type String_Array is array (Integer range <>) of Types.String;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Get_Corbaloc_List
     (Corbaloc      :     Corbaloc_Type;
      Corbaloc_List : out String_Array;
      Tag_List      : out Tag_Array;
      N             : out Natural);
   --  Return the list of all corbaloc obj_addrs found in Corbaloc

   function String_To_Profile
     (Obj_Addr : String) return Binding_Data.Profile_Access;
   --  Return null if failed

   function Profile_To_String
     (P : Binding_Data.Profile_Access) return Types.String;

   function String_To_Object (Str : String) return Corbaloc_Type;

   -----------------------
   -- Get_Corbaloc_List --
   -----------------------

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
     (P : Binding_Data.Profile_Access) return Types.String
   is
      use PolyORB.Types;

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
               return To_PolyORB_String (Info.Profile_To_String_Body (P));
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
     (Obj_Addr : String) return Binding_Data.Profile_Access
   is
      use PolyORB.Utils;

      Prot_Id : String_Ptr;
      Sep     : Integer := Find (Obj_Addr, Obj_Addr'First, ':');

      Iter : Iterator := First (Callbacks);
   begin
      pragma Debug (O ("String_To_Profile: enter, parsing " & Obj_Addr));

      if Obj_Addr (Obj_Addr'First .. Obj_Addr'First + 1) = "//"
        or else (Sep = Obj_Addr'First and then Sep <= Obj_Addr'Last)
      then
         Prot_Id := new String'("iiop");
         if Obj_Addr (Obj_Addr'First) = '/' then
            Sep := Obj_Addr'First + 1;
         end if;
      elsif Sep in Obj_Addr'First + 1 .. Obj_Addr'Last then
         Prot_Id := new String'((Obj_Addr (Obj_Addr'First .. Sep - 1)));
      else
         return null;
      end if;

      while Iter /= Last (Callbacks) loop
         if Prot_Id.all = Value (Iter).Proto_Ident.all then
            pragma Debug
              (O ("Try to unmarshall profile with profile factory tag "
                  & Profile_Tag'Image (Value (Iter).Tag)));
            Free (Prot_Id);
            return Value (Iter).String_To_Profile_Body
              (Obj_Addr (Sep + 1 .. Obj_Addr'Last));
         end if;
         Next (Iter);
      end loop;
      Free (Prot_Id);
      pragma Debug (O ("Profile not found for " & Obj_Addr));
      return null;
   end String_To_Profile;

   ----------------------------------------
   -- Object_To_String_With_Best_Profile --
   ----------------------------------------

   function Object_To_String_With_Best_Profile
     (Corbaloc : Corbaloc_Type) return String is
   begin
      pragma Debug (O ("Create corbaloc with best profile: Enter"));

      if Is_Nil (Corbaloc) then
         pragma Debug (O ("Corbaloc Empty"));
         return Corbaloc_Prefix;
      else
         declare
            use PolyORB.Types;

            N : Natural;
            TL : Tag_Array (1 .. Length (Callbacks));
            SL : String_Array (1 .. Length (Callbacks));
            Profs : constant Profile_Array := Profiles_Of (Corbaloc);
            Best_Preference : Profile_Preference := Profile_Preference'First;
            Best_Profile_Index : Integer := 0;
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
               return Corbaloc_Prefix
                 & To_Standard_String (SL (Best_Profile_Index));
            end if;

            pragma Debug (O ("Create corbaloc with best profile: Leave"));
            return Corbaloc_Prefix;
         end;
      end if;
   end Object_To_String_With_Best_Profile;

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object (Str : String) return Corbaloc_Type is
      use PolyORB.Types;

      Result : Corbaloc_Type;
      Pro    : Profile_Access;
   begin
      pragma Debug (O ("Try to decode Corbaloc: enter "));
      if Utils.Has_Prefix (Str, Corbaloc_Prefix) then
         Pro := String_To_Profile
           (Str (Corbaloc_Prefix'Length + Str'First .. Str'Last));
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
     (Tag                    : PolyORB.Binding_Data.Profile_Tag;
      Proto_Ident            : String;
      Profile_To_String_Body : Profile_To_String_Body_Type;
      String_To_Profile_Body : String_To_Profile_Body_Type) is
   begin
      Append (Callbacks,
              Profile_Record'(Tag,
                              new String'(Proto_Ident),
                              Profile_To_String_Body,
                              String_To_Profile_Body));
   end Register;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_String_To_Object (Corbaloc_Prefix, String_To_Object'Access);
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
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.References.Corbaloc;
