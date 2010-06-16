------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . D N S . U D N S           --
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
with PolyORB.DNS.Transport_Mechanisms.UDNS;
with PolyORB.Initialization;
with PolyORB.Log;
with Ada.Streams;
with PolyORB.Parameters;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Sockets;
with PolyORB.Utils;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Sockets;
with PolyORB.ORB;
with PolyORB.Setup.UDNS;

package body PolyORB.Binding_Data.DNS.UDNS is
   use PolyORB.DNS.Transport_Mechanisms;
   use PolyORB.DNS.Transport_Mechanisms.UDNS;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.Corbaloc;
   use PolyORB.References.IOR;
   use PolyORB.Utils;
   use PolyORB.Utils.Sockets;
   use Ada.Streams;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.dns.udns");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   UDNS_Corbaloc_Prefix : constant String := "udns";

   Preference : Profile_Preference;

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : UDNS_Profile_Type)
     return Profile_Tag
   is
      pragma Unreferenced (Profile);

   begin
      return Tag_UDNS;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : UDNS_Profile_Type)
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
     (PF  : out UDNS_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Unreferenced (ORB);

      MF : constant Transport_Mechanism_Factory_Access
        := new UDNS_Transport_Mechanism_Factory;

   begin
      Create_Factory (MF.all, TAP);
      Append (PF.Mechanisms, MF);
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (PF  : access UDNS_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
 is
      Result : constant Profile_Access := new UDNS_Profile_Type;

      TResult : UDNS_Profile_Type renames UDNS_Profile_Type (Result.all);
   begin
      TResult.Object_Id     := new Object_Id'(Oid);
      --  Create transport mechanism
      Append
        (TResult.Mechanisms,
         Create_Transport_Mechanism
         (UDNS_Transport_Mechanism_Factory
          (Element (PF.Mechanisms, 0).all.all)));
      return Result;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile (P : UDNS_Profile_Type) return Profile_Access is
      Result : constant Profile_Access := new UDNS_Profile_Type;

      TResult : UDNS_Profile_Type renames UDNS_Profile_Type (Result.all);

   begin
      TResult.Object_Id     := new Object_Id'(P.Object_Id.all);
      TResult.Mechanisms    := P.Mechanisms;
      return Result;
   end Duplicate_Profile;

   ---------------------------------
   -- Marshall_UDNS_Profile_Body --
   ---------------------------------

   procedure Marshall_UDNS_Profile_Body
     (Buf     : access Buffer_Type;
      Profile : Profile_Access)
   is
      pragma Unreferenced (Buf);
      Sock : constant Socket_Name := Address_Of
         (UDNS_Transport_Mechanism
            (Element (UDNS_Profile_Type (Profile.all).Mechanisms, 0).all.all));
--        DNS_Profile : DNS_Profile_Type'Class
--        renames DNS_Profile_Type'Class (Profile.all);
      Profile_Body : Buffer_Access := new Buffer_Type;
   begin
--     Start_Encapsulation (Profile_Body);
      Marshall_Socket (Profile_Body, Sock);
--  Marshall (Profile_Body, Stream_Element_Array (DNS_Profile.Object_Id.all));
--     Marshall (Buffer, Encapsulate (Profile_Body));
      Release (Profile_Body);
   end Marshall_UDNS_Profile_Body;

   -----------------------------------
   -- Unmarshall_UDNS_Profile_Body --
   -----------------------------------

   function Unmarshall_UDNS_Profile_Body
     (Buffer : access Buffer_Type) return Profile_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (Buffer);
      Result   : Profile_Access := new UDNS_Profile_Type;
      Address : constant Socket_Name := Address_Of
         (UDNS_Transport_Mechanism
           (Element (UDNS_Profile_Type (Result.all).Mechanisms, 0).all.all));
   begin
      pragma Debug (C, O ("Unmarshall_UDNS_Profile_body: enter"));
      --  Create transport mechanism
      Append
        (UDNS_Profile_Type (Result.all).Mechanisms,
         Create_Transport_Mechanism (Address));
      return Result;
   end Unmarshall_UDNS_Profile_Body;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
      use PolyORB.Sockets;
      use PolyORB.Utils;

      UDNS_Profile : UDNS_Profile_Type
      renames UDNS_Profile_Type (P.all);
      Prefix : constant String := UDNS_Corbaloc_Prefix;
      Oid_Str : String (1 .. P.Object_Id'Length);
      pragma Import (Ada, Oid_Str);
      for Oid_Str'Address use
        P.Object_Id (P.Object_Id'First)'Address;
   begin
      pragma Debug (C, O ("UDNS_Profile_To_Corbaloc"));
      return Prefix & ":@" & Utils.Sockets.Image
          (Address_Of
           (UDNS_Transport_Mechanism (Element
            (UDNS_Profile.Mechanisms, 0).all.all)))
          & "/" & URI_Encode (Oid_Str, Also_Escape => No_Escape);
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      use PolyORB.Utils;

      Profile  : Profile_Access := new UDNS_Profile_Type;
      TResult : UDNS_Profile_Type
        renames UDNS_Profile_Type (Profile.all);

      Host_First, Host_Last : Natural;
      Port : Sockets.Port_Type;
      Empty_Name : constant Socket_Name := "" + 0;
      --  Returned in error case

      S       : String renames Str;
      Index   : Integer := S'First;
      Index2  : Integer;
--      Temp_Ref : TC_Group_Info_Access;
   begin
      pragma Debug (C, O ("UDNS corbaloc to profile: enter"));
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
         declare
            Oid_Str : constant String := URI_Decode (S (Index .. S'Last));
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
         Address : Utils.Sockets.Socket_Name := S (Host_First .. Host_Last)
           + Port;
      begin
         Append
         (UDNS_Profile_Type (Profile.all).Mechanisms,
          Create_Transport_Mechanism (Address));
         return Profile;
      end;
   end Corbaloc_To_Profile;

   -----------
   -- Image --
   -----------

   function Image (Prof : UDNS_Profile_Type) return String is
   begin
      return "Address : "
          & Utils.Sockets.Image
          (Address_Of
          (UDNS_Transport_Mechanism (Element (Prof.Mechanisms, 0).all.all)))
          & ", Object_Id : "
          & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : UDNS_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);

   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   end Get_OA;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Preference_Offset : constant String
        := PolyORB.Parameters.Get_Conf
        (Section => "udns",
         Key     => "polyorb.binding_data.udns.preference",
         Default => "0");

   begin

      Preference := Preference_Default - 1 + Profile_Preference'Value
        (Preference_Offset);
      Register
       (Tag_UDNS,
        Marshall_UDNS_Profile_Body'Access,
        Unmarshall_UDNS_Profile_Body'Access);
      Register
        (Tag_UDNS,
         UDNS_Corbaloc_Prefix,
         Profile_To_Corbaloc'Access,
         Corbaloc_To_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.udns",
       Conflicts => Empty,
       Depends   =>  +"protocols.dns.udns" & "sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.DNS.UDNS;
