------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . R E F E R E N C E S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  Object references.

with Ada.Strings.Unbounded;
with Ada.Tags;

with PolyORB.Binding_Objects;
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.References is

   use type PolyORB.Components.Component_Access;
   use PolyORB.Binding_Objects;
   use PolyORB.Log;
   use PolyORB.Smart_Pointers;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------------------
   -- System location management --
   --------------------------------

   type Prefix_Info is record
      Prefix : Utils.Strings.String_Ptr;
      Func   : String_To_Object_Func;
   end record;

   package Prefix_Info_Lists is
     new PolyORB.Utils.Chained_Lists (Prefix_Info);

   Prefixes : Prefix_Info_Lists.List;

   --------------
   -- Register --
   --------------

   procedure Register_String_To_Object
     (Prefix : String;
      Func   : String_To_Object_Func) is
   begin
      Prefix_Info_Lists.Append (Prefixes,
                                Prefix_Info'(Prefix => new String'(Prefix),
                                             Func   => Func));
      pragma Debug (O ("register prefix: " & Prefix));
   end Register_String_To_Object;

   ----------------------
   -- Create_Reference --
   ----------------------

   procedure Create_Reference
     (Profiles :     Profile_Array;
      Type_Id  :     String;
      R        : out Ref)
   is
      use type Binding_Data.Profile_Access;

   begin
      if Profiles'Length = 0 then
         Set (R, null);
      else
         for J in Profiles'Range loop
            null;
            pragma Assert (Profiles (J) /= null);
         end loop;

         declare
            RIP : constant Entity_Ptr := new Reference_Info;
            TRIP : Reference_Info renames Reference_Info (RIP.all);
         begin
            TRIP.Type_Id  := new String'(Type_Id);
            TRIP.Profiles := new Profile_Array'(Profiles);
            Set (R, RIP);
         end;
      end if;

      pragma Debug (O ("New " & Image (R)));
   end Create_Reference;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (RI : in out Reference_Info) is
   begin
      pragma Debug (O ("Finalize (Reference_Info): enter"));

      Free (RI.Type_Id);
      for J in RI.Profiles'Range loop
         pragma Debug
           (O ("Destroying profile of type "
               & Ada.Tags.External_Tag (RI.Profiles (J)'Tag)));
         Binding_Data.Destroy_Profile (RI.Profiles (J));
      end loop;

      Free (RI.Profiles);

      Annotations.Destroy (RI.Notepad);

      pragma Debug (O ("Finalize (Reference_Info): leave"));
   end Finalize;

   ----------------------
   -- Get_Binding_Info --
   ----------------------

   procedure Get_Binding_Info
     (R   :     Ref'Class;
      BOC : out Components.Component_Access;
      Pro : out Binding_Data.Profile_Access)
   is
      RI : constant Reference_Info_Access
        := Ref_Info_Of (R);

   begin
      if Is_Nil (RI.Binding_Object_Ref) then
         pragma Debug (O ("Get_Binding_Info: Reference is not bound"));
         BOC := null;
         Pro := null;
      else
         BOC := Get_Component (RI.Binding_Object_Ref);
         Pro := RI.Binding_Object_Profile;
      end if;
   end Get_Binding_Info;

   -----------
   -- Image --
   -----------

   function Image
     (R : Ref)
     return String
   is
      use Ada.Strings.Unbounded;

      P : constant Profile_Array := Profiles_Of (R);
      Res : Unbounded_String
        := To_Unbounded_String ("Object reference: ");
   begin
      if P'Length = 0 then
         Res := Res & "<nil or invalid reference>";
      else
         Res := Res & Type_Id_Of (R) & ASCII.LF;

         for J in P'Range loop
            Res := Res & "  " & Ada.Tags.External_Tag
              (P (J).all'Tag) & ASCII.LF;
            Res := Res & "    " & Binding_Data.Image (P (J).all)
              & ASCII.LF;
         end loop;
      end if;

      return To_String (Res);
   end Image;

   ---------------------------
   -- Is_Exported_Reference --
   ---------------------------

   function Is_Exported_Reference (The_Ref : in Ref) return Boolean is
   begin
      if not Is_Nil (The_Ref) then
         return Entity_Of (The_Ref).all in Reference_Info'Class;
      else
         return False;
      end if;
   end Is_Exported_Reference;

   --------------------
   -- Is_Same_Object --
   --------------------

   function Is_Same_Object
     (Left, Right : Ref)
     return Boolean
   is
      use PolyORB.Binding_Data;
      use PolyORB.Objects;

      Left_RI : constant Reference_Info_Access := Ref_Info_Of (Left);
      Right_RI : constant Reference_Info_Access := Ref_Info_Of (Right);

      I_Result : constant Boolean := Left_RI.Type_Id.all = Right_RI.Type_Id.all
        and then Left_RI.Profiles'Length = Right_RI.Profiles'Length;

      Result : Boolean := True;
   begin
      if I_Result then
         for J in Left_RI.Profiles'Range loop
            Result := Result and
              Get_Object_Key (Left_RI.Profiles (J).all).all =
              Get_Object_Key (Right_RI.Profiles (J).all).all;
            --  XXX is this sufficient ??

            --  XXX We cannot compare directly profiles sequence as it
            --  contains pointers to actual profiles, and thus has no
            --  meaning in this context.

         end loop;
         return Result;
      else
         return False;
      end if;
   end Is_Same_Object;

   -----------------
   -- Profiles_Of --
   -----------------

   function Profiles_Of
     (R : Ref)
     return Profile_Array
   is
      RI : constant Reference_Info_Access := Ref_Info_Of (R);

   begin
      if RI /= null then
         return RI.Profiles.all;
      else
         declare
            Null_Profile_Array : Profile_Array (1 .. 0);
         begin
            return Null_Profile_Array;
         end;
      end if;
   end Profiles_Of;

   -----------------
   -- Ref_Info_Of --
   -----------------

   function Ref_Info_Of
     (R : Ref'Class)
     return Reference_Info_Access
   is
      E : constant Entity_Ptr := Entity_Of (R);

   begin
      if E /= null then
         if E.all in Reference_Info'Class then
            return Reference_Info_Access (E);
         else
            pragma Debug (O ("Ref_Info_Of: entity is a "
                             & Ada.Tags.External_Tag (E'Tag)));
            --  XXX does it make sense to have a non-child of
            --  Reference_Info stored into a PolyORB.ReferenceS.Ref ?

            null;
         end if;
      else
         pragma Debug (O ("Ref_Info_Of: nil ref."));
         null;
      end if;

      return null;
   end Ref_Info_Of;

   ------------------------
   -- Share_Binding_Info --
   ------------------------

   procedure Share_Binding_Info
     (Dest   : Ref'Class;
      Source : Ref'Class)
   is
      RD : constant Reference_Info_Access := Ref_Info_Of (Dest);
      RS : constant Reference_Info_Access := Ref_Info_Of (Source);

   begin
      RD.Binding_Object_Ref := RS.Binding_Object_Ref;
      RD.Binding_Object_Profile := RS.Binding_Object_Profile;

      if RD.Type_Id'Length = 0 then
         Free (RD.Type_Id);
         RD.Type_Id := new String'(RS.Type_Id.all);
      end if;
   end Share_Binding_Info;

   ----------------
   -- Type_Id_Of --
   ----------------

   function Type_Id_Of
     (R : Ref)
     return String is
   begin
      return Ref_Info_Of (R).Type_Id.all;
      --  XXX Perhaps some cases of R not designating
      --  a ref_info should be supported here?
   end Type_Id_Of;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object (Str : String; The_Ref : out Ref)
   is
      use Prefix_Info_Lists;

      It : Iterator := First (Prefixes);
   begin
      while not Last (It) loop
         declare
            Prefix : String renames Value (It).Prefix.all;
         begin
            if Utils.Has_Prefix (Str, Prefix) then
               Set (The_Ref, Entity_Of (Value (It).Func (Str)));
               return;
            end if;
         end;
         Next (It);
      end loop;
      raise Constraint_Error;
   end String_To_Object;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (R : in Ref)
     return Annotations.Notepad_Access
   is
   begin
      return Ref_Info_Of (R).Notepad'Access;
   end Notepad_Of;

end PolyORB.References;
