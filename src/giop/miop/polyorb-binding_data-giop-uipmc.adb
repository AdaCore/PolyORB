------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . G I O P . U I P M C       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Binding data concrete implementation for MIOP.

with PolyORB.Binding_Data.GIOP.INET;
with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.GIOP_P.Transport_Mechanisms.UIPMC;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.MIOP_P.Tagged_Components;
with PolyORB.MIOP_P.Groups;
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

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.giop.uipmc");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   UIPMC_Corbaloc_Prefix : constant String := "miop";

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for UIPMC profiles.

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : UIPMC_Profile_Type)
     return Profile_Tag
   is
      pragma Unreferenced (Profile);

   begin
      return Tag_UIPMC;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : UIPMC_Profile_Type)
     return Profile_Preference
   is
      pragma Unreferenced (Profile);

   begin
      return Preference;
   end Get_Profile_Preference;

   --------------------
   -- Create_Factory --
   --------------------

   procedure Create_Factory
     (PF  : out UIPMC_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Unreferenced (ORB);

      MF : constant Transport_Mechanism_Factory_Access
        := new UIPMC_Transport_Mechanism_Factory;

   begin
      Create_Factory (MF.all, TAP);
      Append (PF.Mechanisms, MF);
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (PF  : access UIPMC_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
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
         TResult.Version_Minor := UIPMC_Version_Minor;
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

   function Duplicate_Profile
     (P : UIPMC_Profile_Type)
     return Profile_Access
   is
      Result : constant Profile_Access := new UIPMC_Profile_Type;

      TResult : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (Result.all);

      PP : UIPMC_Profile_Type renames P;

   begin
      TResult.Version_Major := PP.Version_Major;
      TResult.Version_Minor := PP.Version_Minor;
      TResult.Object_Id     := new Object_Id'(PP.Object_Id.all);
      TResult.Components    :=
        PolyORB.GIOP_P.Tagged_Components.Deep_Copy (PP.Components);
      TResult.Mechanisms    := Deep_Copy (PP.Mechanisms);
      TResult.G_I           :=
        new PolyORB.MIOP_P.Groups.Group_Info'(PP.G_I.all);

      return Result;
   end Duplicate_Profile;

   ---------------------------------
   -- Marshall_UIPMC_Profile_Body --
   ---------------------------------

   procedure Marshall_UIPMC_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access)
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
     (Buffer       : access Buffer_Type)
     return Profile_Access
   is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;

      Result   : Profile_Access := new UIPMC_Profile_Type;
      TResult  : UIPMC_Profile_Type renames UIPMC_Profile_Type (Result.all);
      Address  : Sockets.Sock_Addr_Type;
      Temp_Ref : Tagged_Component_Access;

   begin
      pragma Debug (O ("Unmarshall_UIPMC_Profile_body: enter"));

      Common_Unmarshall_Profile_Body
        (Buffer, Result, Address, False, True);

      --  Create transport mechanism

      Append
        (UIPMC_Profile_Type (Result.all).Mechanisms,
         Create_Transport_Mechanism (Address));

      if TResult.Version_Major /= UIPMC_Version_Major then
         Destroy_Profile (Result);
         raise MIOP_Error;
      end if;

      if TResult.Version_Minor /= UIPMC_Version_Minor then
         --  XXX for TAO compatibility, minor version is not check
         --  TAO send profile with UIPMC minor version set to 2 !?
         pragma Debug (O ("Wrong UIPMC minor version :"
                            & TResult.Version_Minor'Img, Warning));
         null;
         --  raise MIOP_Error;
      end if;

      Temp_Ref := Get_Component (TResult.Components, Tag_Group);
      if Temp_Ref = null then
         Destroy_Profile (Result);
         return null;
      end if;
      TResult.G_I := TC_Group_Info_Access (Temp_Ref).G_I'Access;

      TResult.Object_Id := To_Object_Id (TResult.G_I.all);

      pragma Debug (O ("Unmarshall_UIPMC_Profile_body: leave"));

      return Result;
   end Unmarshall_UIPMC_Profile_Body;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Sockets;
      use PolyORB.Utils;

      UIPMC_Profile : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (P.all);

      TC_G_I : constant Tagged_Component_Access
        := Get_Component (UIPMC_Profile.Components, Tag_Group);

   begin
      pragma Debug (O ("UIPMC Profile to corbaloc"));

      if TC_G_I = null then
         return "";
      end if;

      declare
         S : constant String := To_String (TC_Group_Info_Access (TC_G_I));

      begin
         if S = "" then
            return "";
         end if;

         return UIPMC_Corbaloc_Prefix &
           Trimmed_Image (Integer (UIPMC_Profile.Version_Major)) & "."
           & Trimmed_Image (Integer (UIPMC_Profile.Version_Minor)) & "@"
           & S & "/"
           & Image
           (Address_Of
            (UIPMC_Transport_Mechanism
             (Element (UIPMC_Profile.Mechanisms, 0).all.all))) & ":"
           & Trimmed_Image
           (Integer
            (Address_Of
             (UIPMC_Transport_Mechanism
              (Element (UIPMC_Profile.Mechanisms, 0).all.all)).Port));
      end;
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Utils;
      use PolyORB.Utils.Sockets;

      Result  : Profile_Access := new UIPMC_Profile_Type;
      TResult : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (Result.all);
      Address : Sockets.Sock_Addr_Type;
      S       : String renames Str;
      Index   : Integer := S'First;
      Index2  : Integer;
      Temp_Ref : TC_Group_Info_Access;
   begin
      pragma Debug (O ("UIPMC corbaloc to profile: enter"));

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
      pragma Debug (O ("Group Info : " & Image (Temp_Ref.G_I)));

      TResult.G_I := Temp_Ref.G_I'Access;
      TResult.Components := Null_Tagged_Component_List;
      Add (TResult.Components, Tagged_Component_Access (Temp_Ref));
      Index := Index2 + 1;

      Index2 := Find (S, Index, ':');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Result);
         return null;
      end if;
      pragma Debug (O ("Address = " & S (Index .. Index2 - 1)));
      Address.Addr := String_To_Addr (S (Index .. Index2 - 1));
      Index := Index2 + 1;

      pragma Debug (O ("Port = " & S (Index .. S'Last)));
      Address.Port := PolyORB.Sockets.Port_Type'Value (S (Index .. S'Last));

      TResult.Object_Id := To_Object_Id (TResult.G_I.all);

      --  Create transport mechanism

      Append (TResult.Mechanisms, Create_Transport_Mechanism (Address));

      pragma Debug (O ("UIPMC corbaloc to profile: leave"));
      return Result;
   end Corbaloc_To_Profile;

   -----------
   -- Image --
   -----------

   function Image (Prof : UIPMC_Profile_Type) return String is
      use PolyORB.Servants.Group_Servants;
      use PolyORB.Sockets;

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

   function Get_OA
     (Profile : UIPMC_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);

   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.Setup.UIPMC.UIPMC_GOA);
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
      Preference := Preference_Default + Profile_Preference'Value
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
       Init      => Initialize'Access));
end PolyORB.Binding_Data.GIOP.UIPMC;
