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
with PolyORB.Binding_Objects;
with PolyORB.Filters.MIOP.MIOP_Out;
with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.MIOP_P.Tagged_Components;
with PolyORB.MIOP_P.Groups;
with PolyORB.ORB;
with PolyORB.Obj_Adapters;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.UIPMC;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Servants;
with PolyORB.Servants.Group_Servants;
with PolyORB.Transport.Datagram.Sockets_Out;
with PolyORB.Transport.Datagram.Sockets_In;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Sockets;

with PolyORB.Setup.UIPMC;

package body PolyORB.Binding_Data.GIOP.UIPMC is

   use PolyORB.Binding_Data.GIOP.INET;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.MIOP_P.Groups;
   use PolyORB.References.Corbaloc;
   use PolyORB.References.IOR;
   use PolyORB.Transport.Datagram.Sockets_Out;
   use PolyORB.Transport.Datagram.Sockets_In;
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

   ------------------
   -- Bind_Profile --
   ------------------

   Mou : aliased PolyORB.Filters.MIOP.MIOP_Out.MIOP_Out_Factory;
   Pro : aliased PolyORB.Protocols.GIOP.UIPMC.UIPMC_Protocol;

   MIOP_Factories : constant PolyORB.Filters.Factory_Array
     := (0 => Mou'Access, 1 => Pro'Access);

   procedure Bind_Profile
     (Profile :     UIPMC_Profile_Type;
      The_ORB :     Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.Filters;
      use PolyORB.ORB;
      use PolyORB.Parameters;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP.UIPMC;
      use PolyORB.Sockets;

      Sock        : Socket_Type;
      Remote_Addr : constant Sock_Addr_Type := Profile.Address;
      TTL         : constant Natural
        := Natural (Get_Conf ("miop", "polyorb.miop.ttl", Default_TTL));

      TE          : constant Transport.Transport_Endpoint_Access
        := new Socket_Out_Endpoint;

   begin
      pragma Debug (O ("Bind UIPMC profile: enter"));

      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode => Socket_Datagram);

      Set_Socket_Option
        (Sock,
         Socket_Level,
         (Reuse_Address, True));

      Set_Socket_Option
        (Sock,
         IP_Protocol_For_IP_Level,
         (Multicast_TTL, TTL));

      Create (Socket_Out_Endpoint (TE.all), Sock, Remote_Addr);

      Set_Allocation_Class (TE.all, Dynamic);

      Binding_Objects.Setup_Binding_Object
        (ORB.ORB_Access (The_ORB),
         TE,
         MIOP_Factories,
         ORB.Client,
         BO_Ref);

      pragma Debug (O ("Bind UIPMC profile: leave"));

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Profile;

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

   begin
      PF.Address := Address_Of (Socket_In_Access_Point (TAP.all));
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
         TResult.Address       := PF.Address;
         TResult.Components    := Null_Tagged_Component_List;

         TC_G_I.G_I := To_Group_Info (Oid_Access);
         TResult.G_I := TC_G_I.G_I'Access;

         --  Add specific tagged component of type Tag_Group

         Add (TResult.Components, Tagged_Component_Access (TC_G_I));
         return Result;
      end;

   end Create_Profile;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access UIPMC_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean
   is
      use type PolyORB.Sockets.Sock_Addr_Type;

   begin
      return P.all in UIPMC_Profile_Type
        and then UIPMC_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   ---------------------------------
   -- Marshall_UIPMC_Profile_Body --
   ---------------------------------

   procedure Marshall_UIPMC_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access)
   is
   begin
      Common_Marshall_Profile_Body
        (Buf, Profile, UIPMC_Profile_Type (Profile.all).Address, False);
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

      Result  : Profile_Access := new UIPMC_Profile_Type;
      TResult : UIPMC_Profile_Type renames UIPMC_Profile_Type (Result.all);

      Temp_Ref       : Tagged_Component_Access;

   begin
      pragma Debug (O ("Unmarshall_UIPMC_Profile_body: enter"));

      Common_Unmarshall_Profile_Body
        (Buffer, Result, TResult.Address, False, True);

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
      use PolyORB.Types;
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
           & Image (UIPMC_Profile.Address.Addr) & ":"
           & Trimmed_Image (Integer (UIPMC_Profile.Address.Port));
      end;
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Types;
      use PolyORB.Utils;
      use PolyORB.Utils.Sockets;

      Result  : Profile_Access := new UIPMC_Profile_Type;
      TResult : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (Result.all);
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
      TResult.Address.Addr := String_To_Addr
        (To_PolyORB_String (S (Index .. Index2 - 1)));
      Index := Index2 + 1;

      pragma Debug (O ("Port = " & S (Index .. S'Last)));
      TResult.Address.Port
        := PolyORB.Sockets.Port_Type'Value (S (Index .. S'Last));

      TResult.Object_Id := To_Object_Id (TResult.G_I.all);

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
           & Image (Prof.Address)
           & ", Group : "
           & Image (Prof.G_I.all);
      else
         return "Address : "
           & Image (Prof.Address)
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
