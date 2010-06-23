------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . D N S . M D N S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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
with PolyORB.DNS.Transport_Mechanisms;
with PolyORB.DNS.Transport_Mechanisms.MDNS;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.References.Corbaloc;
with PolyORB.Sockets;
with PolyORB.Utils;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Sockets;
with PolyORB.Obj_Adapters;
with PolyORB.Servants.Group_Servants;
with PolyORB.Setup.MDNS;
with PolyORB.Types;
package body PolyORB.Binding_Data.DNS.MDNS is
   use PolyORB.DNS.Transport_Mechanisms;
   use PolyORB.DNS.Transport_Mechanisms.MDNS;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.Corbaloc;
   use PolyORB.Utils;
   use PolyORB.Utils.Sockets;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.dns.mdns");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   MDNS_Corbaloc_Prefix : constant String := "mdns";

   Preference : Profile_Preference;

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : MDNS_Profile_Type)
     return Profile_Tag
   is
      pragma Unreferenced (Profile);

   begin
      return Tag_MDNS;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : MDNS_Profile_Type)
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
     (PF  : out MDNS_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Unreferenced (ORB);

      MF : constant Transport_Mechanism_Factory_Access
        := new MDNS_Transport_Mechanism_Factory;

   begin
      Create_Factory (MF.all, TAP);
      Append (PF.Mechanisms, MF);
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (PF  : access MDNS_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
   is
      use PolyORB.Errors;
      use PolyORB.Obj_Adapters;
      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;

      GS       : PolyORB.Servants.Servant_Access;
      Oid_Access : Object_Id_Access := new Object_Id'(Oid);
      Error    : Error_Container;

   begin
      pragma Debug (C, O ("enter:Oid = " & Image (Oid_Access.all)));
      Find_Servant
        (Obj_Adapter_Access (PolyORB.Setup.MDNS.MDNS_GOA),
         Oid_Access, GS, Error);
      pragma Debug (C, O ("After Find_Servant"));
      if Found (Error) then
         pragma Debug (C, O ("AN ERROR IS FOUND"));
         Free (Oid_Access);
         return null;
      end if;
      pragma Debug (C, O ("Before Get_Group_Object_Id"));
      Get_Group_Object_Id (GS, Oid_Access, Error);
      pragma Debug (C, O ("After Get_Group_Object_Id"));
      if Found (Error)
        or else Oid /= Oid_Access.all
      then
         pragma Debug (C, O ("AN ERROR IS FOUND"));
         Free (Oid_Access);
         return null;
      end if;

      declare
         Result : constant Profile_Access := new MDNS_Profile_Type;
         TResult : MDNS_Profile_Type renames MDNS_Profile_Type (Result.all);
      begin
         TResult.Object_Id := Oid_Access;
         --  Create transport mechanism
         Append
           (TResult.Mechanisms,
            Create_Transport_Mechanism
            (MDNS_Transport_Mechanism_Factory
                 (Element (PF.Mechanisms, 0).all.all)));
         TResult.G_I.all := To_Group_Info (Oid_Access);
         pragma Debug (C, O ("Create:Oid = " & Image (TResult.Object_Id.all)));
         return Result;
      end;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile (P : MDNS_Profile_Type) return Profile_Access is
      Result : constant Profile_Access := new MDNS_Profile_Type;

      TResult : MDNS_Profile_Type renames MDNS_Profile_Type (Result.all);

   begin
      TResult.Object_Id     := new Object_Id'(P.Object_Id.all);
      TResult.Mechanisms    := P.Mechanisms;
      return Result;
   end Duplicate_Profile;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
      use PolyORB.Sockets;

      MDNS_Profile : MDNS_Profile_Type
      renames MDNS_Profile_Type (P.all);
      Prefix : constant String := MDNS_Corbaloc_Prefix;
      Oid_Str : String (1 .. P.Object_Id'Length);
      pragma Import (Ada, Oid_Str);
      for Oid_Str'Address use
        P.Object_Id (P.Object_Id'First)'Address;
   begin
      pragma Debug (C, O ("MDNS_Profile_To_Corbaloc"));
      return Prefix & ":@" & Utils.Sockets.Image
          (Address_Of
           (MDNS_Transport_Mechanism (Element
            (MDNS_Profile.Mechanisms, 0).all.all)))
        & "/TestDomain-1234";
--        & URI_Encode (Oid_Str, Also_Escape => No_Escape);
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is

      Profile  : Profile_Access := new MDNS_Profile_Type;
      TResult : MDNS_Profile_Type
        renames MDNS_Profile_Type (Profile.all);

      Host_First, Host_Last : Natural;
      Port : Sockets.Port_Type;
      --  Returned in error case

      S       : String renames Str;
      Index   : Integer := S'First;
      Index2  : Integer;

   begin
      pragma Debug (C, O ("MDNS corbaloc to profile: enter"));
      Index := Find (S, S'First, '@') + 1;
      --  Index at start of host

      declare
         Colon : constant Integer := Find (S, Index, ':');
         Slash : constant Integer := Find (S, Index, '/');
      begin
         if Colon < Slash then
            --  Port number is present
            Index2 := Colon - 1;
         else
            Index2 := Slash - 1;
         end if;

         if Index2 < Index then
            --  Empty host
            Destroy_Profile (Profile);
            return null;
         end if;
         pragma Debug (C, O ("Address = " & S (Index .. Index2)));
         Host_First := Index;
         Host_Last  := Index2;

         if Colon < Slash then
            if Colon + 1 < Slash then
               pragma Debug (C, O ("Port = " & S (Colon + 1 .. Slash - 1)));
               Port :=
                 PolyORB.Sockets.Port_Type'Value (S (Colon + 1 .. Slash - 1));
            else
               --  Empty port
               Destroy_Profile (Profile);
               return null;
            end if;
         else
            Port := 5353;
         end if;
         Index := Slash + 1;
      end;

      if Index > S'Last then
         --  Empty key_string
         Destroy_Profile (Profile);
         return null;
      end if;
      TResult.G_I := new Group_Info;
      TResult.G_I.Group_Domain_Id := To_PolyORB_String ("TestDomain");
      TResult.G_I.Object_Group_Id := 5252;
      pragma Debug (C, O ("Before TO_OBJECT_ID"));
      TResult.Object_Id := To_Object_Id (TResult.G_I.all);
      pragma Debug (C, O ("After TO_OBJECT_ID"));
      if TResult.Object_Id = null then
         pragma Debug (C, O ("Object ID IS NULL"));
         Destroy_Profile (Profile);
         return null;
      end if;

      pragma Debug (C, O ("Oid = " & Image (TResult.Object_Id.all)));

      declare
         Address : constant Utils.Sockets.Socket_Name
           := S (Host_First .. Host_Last) + Port;
      begin
         Append
         (MDNS_Profile_Type (Profile.all).Mechanisms,
         Create_Transport_Mechanism (Address));
         pragma Debug (C, O ("MDNS corbaloc to profile: leave"));
         return Profile;
      end;
   end Corbaloc_To_Profile;

   -----------
   -- Image --
   -----------

   function Image (Prof : MDNS_Profile_Type) return String is
   begin
      return "Address : "
          & Utils.Sockets.Image
          (Address_Of
          (MDNS_Transport_Mechanism (Element (Prof.Mechanisms, 0).all.all)))
          & ", Object_Id : "
          & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : MDNS_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);

   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.Setup.MDNS.MDNS_GOA);
   end Get_OA;

   function From_String
     (S : String)
     return Group_Info_Access
   is
      use PolyORB.Utils.Strings;

      Index  : Integer := S'First;
      Index2 : Integer;
      G_I    : constant Group_Info_Access := new Group_Info;

   begin
      pragma Debug (C, O ("Extract Group_Info from string"));

      Index2 := Find (S, Index, '.');
      if Index2 = S'Last + 1 then
         return null;
      end if;

      Index := Index2 + 1;

      Index2 := Find (S, Index, '-');
      if Index2 = S'Last + 1 then
         return null;
      end if;

      Index := Index2 + 1;

      Index2 := Find (S, Index, '-');
      if Index2 = S'Last + 1 then
         return null;
      end if;

      G_I.Group_Domain_Id := To_PolyORB_String (S (Index .. Index2 - 1));
      Index := Index2 + 1;

      Index2 := Find (S, Index, '-');
      if Index2 = S'Last + 1 then
         G_I.Object_Group_Id
           := Types.Unsigned_Long_Long'Value (S (Index .. S'Last));

      else
         G_I.Object_Group_Id
           := Types.Unsigned_Long_Long'Value (S (Index .. Index2 - 1));
         G_I.Object_Group_Ref_Version
           := Types.Unsigned_Long'Value (S (Index2 + 1 .. S'Last));
      end if;
--      pragma Debug (C, O ("Group Info : " & Image (G_I)));
      return G_I;
   end From_String;
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Preference_Offset : constant String
        := PolyORB.Parameters.Get_Conf
        (Section => "mdns",
         Key     => "polyorb.binding_data.mdns.preference",
         Default => "0");

   begin

      Preference := Preference_Default - 1 + Profile_Preference'Value
        (Preference_Offset);
      Register
        (Tag_MDNS,
         MDNS_Corbaloc_Prefix,
         Profile_To_Corbaloc'Access,
         Corbaloc_To_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.mdns",
       Conflicts => Empty,
       Depends   =>  +"protocols.dns.mdns" & "sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.DNS.MDNS;
