------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . G I O P . U I P M C       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2017, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  Binding data concrete implementation for MIOP

with PolyORB.Binding_Data.GIOP.INET;
with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.GIOP_P.Transport_Mechanisms.UIPMC;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.MIOP_P.Tagged_Components;
with PolyORB.Obj_Adapters;
with PolyORB.Parameters;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Servants;
with PolyORB.Servants.Group_Servants;
with PolyORB.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Sockets;

with PolyORB.Setup.UIPMC;

package body PolyORB.Binding_Data.GIOP.UIPMC is

   use PolyORB.Binding_Data.GIOP.INET;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.GIOP_P.Transport_Mechanisms.UIPMC;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.MIOP_P.Groups;
   use PolyORB.References.Corbaloc;
   use PolyORB.References.IOR;
   use PolyORB.Types;
   use PolyORB.Utils.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.giop.uipmc");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   UIPMC_Corbaloc_Prefix : constant String := "miop";

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for UIPMC profiles.

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   overriding function Get_Profile_Tag
     (Profile : UIPMC_Profile_Type) return Profile_Tag
   is
      pragma Unreferenced (Profile);

   begin
      return Tag_UIPMC;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   overriding function Get_Profile_Preference
     (Profile : UIPMC_Profile_Type) return Profile_Preference
   is
      pragma Unreferenced (Profile);

   begin
      return Preference;
   end Get_Profile_Preference;

   --------------------
   -- Create_Factory --
   --------------------

   overriding function Create_Factory
     (TAP : not null access Transport.Transport_Access_Point'Class)
      return UIPMC_Profile_Factory
   is
      MF : constant Transport_Mechanism_Factory_Access :=
             new UIPMC_Transport_Mechanism_Factory;

   begin
      return PF : UIPMC_Profile_Factory do
         Create_Factory (MF.all, TAP);
         Append (PF.Mechanisms, MF);
      end return;
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   overriding function Create_Profile
     (PF  : access UIPMC_Profile_Factory;
      Oid :        Objects.Object_Id) return Profile_Access
   is
      use PolyORB.Errors;
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Obj_Adapters;
      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;

      GS         : PolyORB.Servants.Servant_Access;
      Error      : Error_Container;
      Oid_Access : Object_Id_Access := new Object_Id'(Oid);

   begin
      Find_Servant
        (Obj_Adapter_Access (PolyORB.Setup.UIPMC.UIPMC_GOA),
         Oid_Access, GS, Error);

      if Found (Error) then
         Free (Oid_Access);
         return null;
      end if;

      Get_Group_Object_Id (GS, Oid_Access, Error);

      if Found (Error)
        or else Oid /= Oid_Access.all
      then
         Free (Oid_Access);
         return null;
      end if;

      declare
         Result : constant Profile_Access := new UIPMC_Profile_Type;

         TResult : UIPMC_Profile_Type
           renames UIPMC_Profile_Type (Result.all);
         TC_G_I : TC_Group_Info_Access := new TC_Group_Info;

      begin
         TResult.Version_Major := UIPMC_Version_Major;

         --  We force Version_Minor to 2 to match MIOP specifications
         --  that requires MIOP 1.0 profile to be conformant with GIOP
         --  1.2.

         TResult.Version_Minor := 2;

         TResult.Object_Id     := Oid_Access;
         TResult.Components    := Null_Tagged_Component_List;

         --  Create transport mechanism

         Append
           (TResult.Mechanisms,
            Create_Transport_Mechanism
            (UIPMC_Transport_Mechanism_Factory
             (Element (PF.Mechanisms, 0).all.all)));

         TC_G_I.G_I := To_Group_Info (Oid_Access);
         TResult.G_I := TC_G_I.G_I'Access;

         --  Add specific tagged component of type Tag_Group

         Add (TResult.Components, Tagged_Component_Access (TC_G_I));

         return Result;
      end;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   overriding function Duplicate_Profile
     (P : UIPMC_Profile_Type)
     return Profile_Access
   is
      Result  : constant Profile_Access := new UIPMC_Profile_Type;
      TResult : UIPMC_Profile_Type renames UIPMC_Profile_Type (Result.all);

   begin
      TResult.Version_Major := P.Version_Major;
      TResult.Version_Minor := P.Version_Minor;
      TResult.Object_Id     := new Object_Id'(P.Object_Id.all);
      TResult.Components    :=
        PolyORB.GIOP_P.Tagged_Components.Deep_Copy (P.Components);
      TResult.Mechanisms    := Deep_Copy (P.Mechanisms);
      TResult.G_I           :=
        new PolyORB.MIOP_P.Groups.Group_Info'(P.G_I.all);

      return Result;
   end Duplicate_Profile;

   ---------------------------------
   -- Marshall_UIPMC_Profile_Body --
   ---------------------------------

   procedure Marshall_UIPMC_Profile_Body
     (Buf     : access Buffer_Type;
      Profile : Profile_Access)
   is
   begin
      Common_Marshall_Profile_Body
        (Buf,
         Profile,
         Address_Of
         (UIPMC_Transport_Mechanism
          (Element (UIPMC_Profile_Type (Profile.all).Mechanisms, 0).all.all)),
         False);
   end Marshall_UIPMC_Profile_Body;

   -----------------------------------
   -- Unmarshall_UIPMC_Profile_Body --
   -----------------------------------

   function Unmarshall_UIPMC_Profile_Body
     (Buffer : access Buffer_Type) return Profile_Access
   is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;

      Result   : Profile_Access := new UIPMC_Profile_Type;
      TResult  : UIPMC_Profile_Type renames UIPMC_Profile_Type (Result.all);
      Address  : constant Utils.Sockets.Socket_Name :=
        Common_Unmarshall_Profile_Body
          (Buffer,
           Result,
           Unmarshall_Object_Id => False,
           Unmarshall_Tagged_Components => True);

      Temp_Ref : Tagged_Component_Access;

   begin
      pragma Debug (C, O ("Unmarshall_UIPMC_Profile_body: enter"));

      --  Create transport mechanism

      Append
        (UIPMC_Profile_Type (Result.all).Mechanisms,
         Create_Transport_Mechanism (Address));

      if TResult.Version_Major /= UIPMC_Version_Major then
         Destroy_Profile (Result);
         raise MIOP_Error;
      end if;

      --  We force Version_Minor to 2 to match MIOP specifications
      --  that requires MIOP 1.0 profile to be conformant with GIOP
      --  1.2.

      TResult.Version_Minor := 2;

      Temp_Ref := Get_Component (TResult.Components, Tag_Group);
      if Temp_Ref = null then
         Destroy_Profile (Result);
         return null;
      end if;
      TResult.G_I       := TC_Group_Info_Access (Temp_Ref).G_I'Access;
      TResult.Object_Id := To_Object_Id (TResult.G_I.all);

      pragma Debug (C, O ("Unmarshall_UIPMC_Profile_body: leave"));

      return Result;
   end Unmarshall_UIPMC_Profile_Body;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Utils;

      UIPMC_Profile : UIPMC_Profile_Type renames UIPMC_Profile_Type (P.all);

      TC_G_I : constant Tagged_Component_Access :=
        Get_Component (UIPMC_Profile.Components, Tag_Group);

   begin
      pragma Debug (C, O ("UIPMC Profile to corbaloc"));

      if TC_G_I = null then
         return "";
      end if;

      declare
         S : constant String := To_String (TC_Group_Info_Access (TC_G_I));

      begin
         if S = "" then
            return "";
         end if;

         --  Note: we force Version_Minor to 0 to match MIOP
         --  specifications that requires MIOP 1.0 profile to be
         --  conformant with GIOP 1.2.

         return UIPMC_Corbaloc_Prefix
           & ":" & Trimmed_Image (Unsigned_Long_Long
                                  (UIPMC_Profile.Version_Major)) & "."
           & Trimmed_Image (Unsigned_Long_Long
                            (0)) & "@"
           & S & "/"
           & Image
           (Address_Of
            (UIPMC_Transport_Mechanism
             (Element (UIPMC_Profile.Mechanisms, 0).all.all)));
      end;
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Utils;

      Result  : Profile_Access := new UIPMC_Profile_Type;
      TResult : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (Result.all);

      Host_First, Host_Last : Natural;
      Port : Sockets.Port_Type;

      S        : String renames Str;
      Index    : Integer := S'First;
      Index2   : Integer;
      Temp_Ref : TC_Group_Info_Access;
   begin
      pragma Debug (C, O ("UIPMC corbaloc to profile: enter"));

      Index2 := Find (S, Index, '.');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Result);
         return null;
      end if;
      TResult.Version_Major :=
        Types.Octet'Value (S (Index .. Index2 - 1));

      if TResult.Version_Major /= UIPMC_Version_Major then
         Destroy_Profile (Result);
         return null;
      end if;

      Index := Index2 + 1;

      Index2 := Find (S, Index, '@');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Result);
         return null;
      end if;
      TResult.Version_Minor :=
        Types.Octet'Value (S (Index .. Index2 - 1));
      if TResult.Version_Minor /= UIPMC_Version_Minor then
         Destroy_Profile (Result);
         return null;
      end if;

      --  We force Version_Minor to 2 to match MIOP specifications
      --  that requires MIOP 1.0 profile to be conformant with GIOP
      --  1.2.

      TResult.Version_Minor := 2;

      Index := Index2 + 1;

      Index2 := Find (S, Index, '/');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Result);
         return null;
      end if;

      Temp_Ref := From_String (S (Index .. Index2 - 1));
      if Temp_Ref = null then
         Destroy_Profile (Result);
         return null;
      end if;
      pragma Debug (C, O ("Group Info : " & Image (Temp_Ref.G_I)));

      TResult.G_I := Temp_Ref.G_I'Access;
      TResult.Components := Null_Tagged_Component_List;
      Add (TResult.Components, Tagged_Component_Access (Temp_Ref));
      Index := Index2 + 1;

      Index2 := Find (S, Index, ':');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Result);
         return null;
      end if;
      pragma Debug (C, O ("Address = " & S (Index .. Index2 - 1)));
      Host_First := Index;
      Host_Last := Index2 - 1;
      Index := Index2 + 1;

      pragma Debug (C, O ("Port = " & S (Index .. S'Last)));
      Port := PolyORB.Sockets.Port_Type'Value (S (Index .. S'Last));

      TResult.Object_Id := To_Object_Id (TResult.G_I.all);

      --  Create transport mechanism

      Append (TResult.Mechanisms,
        Create_Transport_Mechanism (S (Host_First .. Host_Last) + Port));

      pragma Debug (C, O ("UIPMC corbaloc to profile: leave"));
      return Result;
   end Corbaloc_To_Profile;

   -----------
   -- Image --
   -----------

   overriding function Image (Prof : UIPMC_Profile_Type) return String is
   begin
      if Prof.G_I /= null then
         return "Address : "
           & Image
           (Address_Of
            (UIPMC_Transport_Mechanism
             (Element (Prof.Mechanisms, 0).all.all)))
           & ", Group : "
           & Image (Prof.G_I.all);
      else
         return "Address : "
           & Image (Address_Of
           (UIPMC_Transport_Mechanism
            (Element (Prof.Mechanisms, 0).all.all)))
           & ", no group information";
      end if;
   end Image;

   ------------
   -- Get_OA --
   ------------

   overriding function Get_OA
     (Profile : UIPMC_Profile_Type) return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);

   begin
      return PolyORB.Smart_Pointers.Entity_Ptr (PolyORB.Setup.UIPMC.UIPMC_GOA);
   end Get_OA;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Preference_Offset : constant String
        := PolyORB.Parameters.Get_Conf
        (Section => "miop",
         Key     => "polyorb.binding_data.uipmc.preference",
         Default => "0");

   begin
      --  XXX we impose a slight preference penalty to UIPMC to favor IIOP
      --  by default. See F501-004.

      Preference := Preference_Default - 1 + Profile_Preference'Value
        (Preference_Offset);
      Register
       (Tag_UIPMC,
        Marshall_UIPMC_Profile_Body'Access,
        Unmarshall_UIPMC_Profile_Body'Access);
      Register
        (Tag_UIPMC,
         UIPMC_Corbaloc_Prefix,
         Profile_To_Corbaloc'Access,
         Corbaloc_To_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.uipmc",
       Conflicts => Empty,
       Depends   => +"sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.GIOP.UIPMC;
