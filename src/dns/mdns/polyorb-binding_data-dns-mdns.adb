------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . B I N D I N G _ D A T A . D N S . M D N S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2013, Free Software Foundation, Inc.          --
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

with Ada.Streams;

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

package body PolyORB.Binding_Data.DNS.MDNS is

   use Ada.Streams;
   use PolyORB.DNS.Transport_Mechanisms;
   use PolyORB.DNS.Transport_Mechanisms.MDNS;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.Corbaloc;
   use PolyORB.Utils;
   use PolyORB.Utils.Sockets;

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

   overriding function Get_Profile_Tag
     (Profile : MDNS_Profile_Type) return Profile_Tag
   is
      pragma Unreferenced (Profile);
   begin
      return Tag_MDNS;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   overriding function Get_Profile_Preference
     (Profile : MDNS_Profile_Type) return Profile_Preference
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
     return MDNS_Profile_Factory
   is
      MF : constant Transport_Mechanism_Factory_Access :=
        new MDNS_Transport_Mechanism_Factory;

   begin
      return PF : MDNS_Profile_Factory do
         Create_Factory (MF.all, TAP);
         PF.Mechanism := MF;
      end return;
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   overriding function Create_Profile
     (PF  : access MDNS_Profile_Factory;
      Oid : Objects.Object_Id) return Profile_Access
   is
      Result : constant Profile_Access := new MDNS_Profile_Type;
      TResult : MDNS_Profile_Type renames MDNS_Profile_Type (Result.all);

   begin
      TResult.Object_Id     := new Object_Id'(Oid);
      pragma Debug (C, O ("enter:Oid = " & Image (TResult.Object_Id.all)));

      --  Create transport mechanism

      TResult.Mechanism :=
        Create_Transport_Mechanism
          (MDNS_Transport_Mechanism_Factory (PF.Mechanism.all));

      pragma Debug (C, O ("leave"));
      return Result;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   overriding function Duplicate_Profile
     (P : MDNS_Profile_Type) return Profile_Access
   is
      Result : constant Profile_Access := new MDNS_Profile_Type;

      TResult : MDNS_Profile_Type renames MDNS_Profile_Type (Result.all);

   begin
      TResult.Object_Id := new Object_Id'(P.Object_Id.all);

      TResult.Mechanism := P.Mechanism;
      --  Incorrect aliasing of Mechanism???

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
          (Address_Of (MDNS_Profile.Mechanism.all))  & "/";
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
      pragma Debug (C, O ("MDNS corbaloc to profile: enter: "));
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
      end;

      if Index > S'Last then
         --  Empty key_string
         Destroy_Profile (Profile);
         return null;
      end if;

      declare
         Slash   : constant Integer := Find (S, Index, '/');
         Oid_Str : constant String := URI_Decode (S (Slash + 1 .. S'Last));
         Oid     : Object_Id (Stream_Element_Offset (Oid_Str'First)
                        .. Stream_Element_Offset (Oid_Str'Last));
         pragma Import (Ada, Oid);
         for Oid'Address use Oid_Str (Oid_Str'First)'Address;
      begin
         TResult.Object_Id := new Object_Id'(Oid);
      end;

      if TResult.Object_Id = null then
         Destroy_Profile (Profile);
         return null;
      end if;

      pragma Debug (C, O ("Oid = " & Image (TResult.Object_Id.all)));

      declare
         Address : constant Utils.Sockets.Socket_Name :=
           S (Host_First .. Host_Last) + Port;
      begin
         DNS_Profile_Type (Profile.all).Mechanism :=
           Create_Transport_Mechanism (Address);
         pragma Debug (C, O ("MDNS corbaloc to profile: leave"));
         return Profile;
      end;
   end Corbaloc_To_Profile;

   --------------------------
   -- Is_Multicast_Profile --
   --------------------------

   overriding function Is_Multicast_Profile
     (P : MDNS_Profile_Type) return Boolean
   is
      pragma Unreferenced (P);
   begin
      return True;
   end Is_Multicast_Profile;

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
       Depends   => +"sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.DNS.MDNS;
