------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . U I P M C           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2003 Free Software Foundation, Inc.            --
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

--  Binding data concrete implementation for MIOP.

with PolyORB.Configuration;
with PolyORB.Exceptions;
with PolyORB.Filters;
with PolyORB.Filters.MIOP;
with PolyORB.Filters.MIOP.MIOP_Out;
with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Log;
with PolyORB.MIOP_P.Tagged_Components;
with PolyORB.MIOP_P.Groups;
with PolyORB.ORB;
with PolyORB.Obj_Adapters;
with PolyORB.Protocols;
with PolyORB.Protocols.GIOP.UIPMC;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR;
with PolyORB.Servants;
with PolyORB.Servants.Group_Servants;
with PolyORB.Transport.Datagram.Sockets_Out;
with PolyORB.Transport.Datagram.Sockets_In;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Sockets;

with PolyORB.Setup.UIPMC;

package body PolyORB.Binding_Data.UIPMC is

   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.MIOP_P.Groups;
   use PolyORB.References.Corbaloc;
   use PolyORB.References.IOR;
   use PolyORB.Representations.CDR;
   use PolyORB.Transport.Datagram.Sockets_Out;
   use PolyORB.Transport.Datagram.Sockets_In;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_data.uipmc");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for UIPMC profiles.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (P : in out UIPMC_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (P : in out UIPMC_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (P : in out UIPMC_Profile_Type)
   is
      use PolyORB.GIOP_P.Tagged_Components;
   begin
      pragma Debug (O ("Finalize UIPMC Profile"));
      Free (P.Object_Id);
      Release_Contents (P.Components);
   end Finalize;

   ------------------
   -- Bind_Profile --
   ------------------

   function Bind_Profile
     (Profile : UIPMC_Profile_Type;
      The_ORB : Components.Component_Access)
     return Components.Component_Access
   is
      use PolyORB.Configuration;
      use PolyORB.Components;
      use PolyORB.Filters;
      use PolyORB.Filters.MIOP.MIOP_Out;
      use PolyORB.ORB;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP.UIPMC;
      use PolyORB.Sockets;

      Sock        : Socket_Type;
      Remote_Addr : constant Sock_Addr_Type := Profile.Address;
      TTL         : constant Natural
        := Natural (Get_Conf
                    ("miop", "polyorb.miop.ttl",
                     Default_TTL));
      TE          : constant Transport.Transport_Endpoint_Access
        := new Socket_Out_Endpoint;
      Pro         : aliased UIPMC_Protocol;
      M_Fact      : aliased MIOP_Out_Factory;
      Prof        : constant Profile_Access := new UIPMC_Profile_Type;
      --  This Profile_Access is stored in the created
      --  GIOP_Session, and free'd when the session is finalized.

      Filter : Filters.Filter_Access;

      TProf : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (Prof.all);

   begin
      pragma Debug (O ("Bind UIPMC profile: enter"));

      --  Create Socket
      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode => Socket_Datagram);

      Set_Socket_Option
        (Sock,
         Socket_Level,
         (Reuse_Address, True));


      --  Set TTL value for this socket
      Set_Socket_Option
        (Sock,
         IP_Protocol_For_IP_Level,
         (Multicast_TTL, TTL));

      Create (Socket_Out_Endpoint (TE.all), Sock, Remote_Addr);

      Chain_Factories ((0 => M_Fact'Unchecked_Access,
                        1 => Pro'Unchecked_Access));

      Filter := MIOP.MIOP_Out.Create_Filter_Chain (M_Fact'Unchecked_Access);

      ORB.Register_Endpoint
        (ORB_Access (The_ORB),
         TE,
         Filter,
         ORB.Client);
      --  Register the endpoint and lowest filter with the ORB.

      pragma Debug (O ("Preparing local copy of profile"));
      TProf.Address := Profile.Address;
      TProf.Object_Id := Profile.Object_Id;
      Adjust (TProf);
      pragma Debug (O ("Adjusted local copy of profile"));

      declare
         C : Component
           renames Component (Upper (Filter).all);
      begin
         pragma Debug (O ("Bind UIPMC profile: leave"));
         return C'Access;
      end;
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : UIPMC_Profile_Type)
     return Profile_Tag
   is
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);

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
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);

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
      pragma Warnings (Off);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
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
      use PolyORB.Exceptions;
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Obj_Adapters;
      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;

      GS         : PolyORB.Servants.Servant_Access;
      Error      : Error_Container;
      Oid_Access : Object_Id_Access := new Object_Id'(Oid);
   begin
      Find_Servant (Obj_Adapter_Access (PolyORB.Setup.UIPMC.UIPMC_GOA),
                    Oid_Access,
                    GS,
                    Error);
      if Found (Error) then
         Free (Oid_Access);
         return null;
      end if;
      Get_Group_Object_Id (GS, Oid_Access, Error);
      if Found (Error)
        or else Oid /= Oid_Access.all then
         Free (Oid_Access);
         return null;
      end if;
      declare
         Result : constant Profile_Access
           := new UIPMC_Profile_Type;

         TResult : UIPMC_Profile_Type
           renames UIPMC_Profile_Type (Result.all);
         TC_G_I     : TC_Group_Info_Access := new TC_Group_Info;
      begin
         TResult.Object_Id := Oid_Access;
         TResult.Address := PF.Address;
         TResult.Components := Null_Tagged_Component_List;
         TC_G_I.G_I := To_Group_Info (Oid_Access);
         TResult.G_I := TC_G_I.G_I'Access;
         Add (TResult.Components,
              Tagged_Component_Access (TC_G_I));
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
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.Utils.Sockets;

      UIPMC_Profile : UIPMC_Profile_Type
        renames UIPMC_Profile_Type (Profile.all);

      Profile_Body : Buffer_Access := new Buffer_Type;
   begin
      pragma Debug (O ("Marshall_UIPMC_Profile_body: enter"));

      --  A TAG_UIPMC Profile Body is an encapsulation.
      Start_Encapsulation (Profile_Body);

      --  Version
      Marshall (Profile_Body, UIPMC_Version_Major);
      Marshall (Profile_Body, UIPMC_Version_Minor);

      --  Marshalling of a Socket
      Marshall_Socket (Profile_Body, UIPMC_Profile.Address);

      pragma Debug (O ("  Address = "
                       & Sockets.Image (UIPMC_Profile.Address)));

      --  Marshall the tagged components.
      Marshall_Tagged_Component (Profile_Body, UIPMC_Profile.Components);

      --  Marshall the Profile_Body into IOR.
      Marshall (Buf, Encapsulate (Profile_Body));
      Release (Profile_Body);

      pragma Debug (O ("Marshall_UIPMC_Profile_body: leave"));

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
      use PolyORB.Utils.Sockets;

      Result  : Profile_Access := new UIPMC_Profile_Type;
      TResult : UIPMC_Profile_Type renames UIPMC_Profile_Type (Result.all);

      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access := new Buffers.Buffer_Type;
      Temp_Ref       : Tagged_Component_Access;
      Temp           : Types.Octet;
   begin
      pragma Debug (O ("Unmarshall_UIPMC_Profile_body: enter"));
      Decapsulate (Profile_Body'Access, Profile_Buffer);

      Temp := Unmarshall (Profile_Buffer);
      if Temp /= UIPMC_Version_Major then
         raise MIOP_Error;
      end if;
      Temp := Unmarshall (Profile_Buffer);
      if Temp /= UIPMC_Version_Minor then
         --  XXX for TAO compatibility, minor version is not check
         --  TAO send profile with UIPMC minor version set to 2 !?
         pragma Debug (O ("Wrong UIPMC minor version :" & Temp'Img, Warning));
         null;
         --  raise MIOP_Error;
      end if;

      Unmarshall_Socket (Profile_Buffer, TResult.Address);

      pragma Debug (O ("  Address = " & Sockets.Image (TResult.Address)));

      TResult.Components :=
        Unmarshall_Tagged_Component (Profile_Buffer);

      Temp_Ref := Get_Component (TResult.Components, Tag_Group);
      if Temp_Ref = null then
         Destroy_Profile (Result);
         return null;
      end if;
      TResult.G_I := TC_Group_Info_Access (Temp_Ref).G_I'Access;

      TResult.Object_Id := To_Object_Id (TResult.G_I.all);
      Set_OA (TResult, PolyORB.Setup.UIPMC.UIPMC_GOA_Ref);

      Release (Profile_Buffer);

      pragma Debug (O ("Unmarshall_UIPMC_Profile_body: leave"));

      return Result;
   end Unmarshall_UIPMC_Profile_Body;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc
     (P : Profile_Access)
     return Types.String
   is
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
         return To_PolyORB_String ("");
      end if;
      declare
         S : constant String :=
           To_String (TC_Group_Info_Access (TC_G_I));
      begin
         if S = "" then
            return To_PolyORB_String ("");
         end if;
         return UIPMC_Corbaloc_Prefix & To_PolyORB_String
           (Trimmed_Image (Integer (UIPMC_Version_Major)) & "." &
            Trimmed_Image (Integer (UIPMC_Version_Minor)) & "@" &
            S & "/" &
            Image (UIPMC_Profile.Address.Addr) & ":" &
            Trimmed_Image (Integer (UIPMC_Profile.Address.Port)));
      end;
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile
     (Str : Types.String)
     return Profile_Access
   is
      use PolyORB.GIOP_P.Tagged_Components;
      use PolyORB.MIOP_P.Tagged_Components;
      use PolyORB.Types;
      use PolyORB.Utils;
      use PolyORB.Utils.Sockets;

      Len    : constant Integer := Length (UIPMC_Corbaloc_Prefix);
   begin
      if Length (Str) > Len
        and then To_String (Str) (1 .. Len) = UIPMC_Corbaloc_Prefix then
         declare
            Result  : Profile_Access := new UIPMC_Profile_Type;
            TResult : UIPMC_Profile_Type
              renames UIPMC_Profile_Type (Result.all);
            S       : constant String
              := To_Standard_String (Str) (Len + 1 .. Length (Str));
            Index   : Integer := S'First;
            Index2  : Integer;
            Temp_Ref : TC_Group_Info_Access;
         begin
            pragma Debug (O ("UIPMC corbaloc to profile: enter"));

            Index2 := Find (S, Index, '.');
            if Index2 = S'Last + 1 then
               return null;
            end if;
            if Types.Octet'Value (S (Index .. Index2 - 1))
              /= UIPMC_Version_Major then
               return null;
            end if;
            Index := Index2 + 1;

            Index2 := Find (S, Index, '@');
            if Index2 = S'Last + 1 then
               return null;
            end if;
            if Types.Octet'Value (S (Index .. Index2 - 1))
              /= UIPMC_Version_Minor then
               return null;
            end if;
            Index := Index2 + 1;

            Index2 := Find (S, Index, '/');
            if Index2 = S'Last + 1 then
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
               return null;
            end if;
            pragma Debug (O ("Address = " & S (Index .. Index2 - 1)));
            TResult.Address.Addr := String_To_Addr
              (To_PolyORB_String (S (Index .. Index2 - 1)));
            Index := Index2 + 1;

            pragma Debug (O ("Port = " & S (Index .. S'Last)));
            TResult.Address.Port :=
              PolyORB.Sockets.Port_Type'Value (S (Index .. S'Last));

            TResult.Object_Id := To_Object_Id (TResult.G_I.all);
            Set_OA (TResult, PolyORB.Setup.UIPMC.UIPMC_GOA_Ref);
            pragma Debug (O ("UIPMC corbaloc to profile: leave"));
            return Result;
         end;
      end if;
      return null;
   end Corbaloc_To_Profile;

   -----------
   -- Image --
   -----------

   function Image
     (Prof : UIPMC_Profile_Type)
     return String
   is
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      Preference_Offset : constant String
        := PolyORB.Configuration.Get_Conf
        (Section => "corba",
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
       Init      => Initialize'Access));

end PolyORB.Binding_Data.UIPMC;
