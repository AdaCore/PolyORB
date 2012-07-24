------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . D I O P        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.GIOP.INET;
with PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Sockets;

package body PolyORB.Binding_Data.GIOP.DIOP is

   use PolyORB.Binding_Data.GIOP.INET;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.IOR;
   use PolyORB.References.Corbaloc;
   use PolyORB.Types;
   use PolyORB.Utils.Sockets;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.binding_data.giop.diop");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   DIOP_Corbaloc_Prefix : constant String := "diop";

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for DIOP profiles.

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   overriding function Get_Profile_Tag
     (Profile : DIOP_Profile_Type)
     return Profile_Tag is
      pragma Unreferenced (Profile);

   begin
      return Tag_DIOP;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   overriding function Get_Profile_Preference
     (Profile : DIOP_Profile_Type)
     return Profile_Preference
   is
      pragma Unreferenced (Profile);

   begin
      return Preference;
   end Get_Profile_Preference;

   --------------------
   -- Create_Factory --
   --------------------

   overriding procedure Create_Factory
     (PF  : out DIOP_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Unreferenced (ORB);

      MF : constant Transport_Mechanism_Factory_Access
        := new DIOP_Transport_Mechanism_Factory;

   begin
      Create_Factory (MF.all, TAP);
      Append (PF.Mechanisms, MF);
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   overriding function Create_Profile
     (PF  : access DIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
   is
      Result : constant Profile_Access := new DIOP_Profile_Type;

      TResult : DIOP_Profile_Type renames DIOP_Profile_Type (Result.all);
   begin
      TResult.Version_Major := DIOP_Version_Major;
      TResult.Version_Minor := DIOP_Version_Minor;
      TResult.Object_Id     := new Object_Id'(Oid);
      TResult.Components    := Null_Tagged_Component_List;

      --  Create transport mechanism

      Append
        (TResult.Mechanisms,
         Create_Transport_Mechanism
         (DIOP_Transport_Mechanism_Factory
          (Element (PF.Mechanisms, 0).all.all)));

      return Result;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   overriding function Duplicate_Profile
     (P : DIOP_Profile_Type)
     return Profile_Access is
      Result : constant Profile_Access := new DIOP_Profile_Type;

      TResult : DIOP_Profile_Type renames DIOP_Profile_Type (Result.all);

   begin
      TResult.Version_Major := P.Version_Major;
      TResult.Version_Minor := P.Version_Minor;
      TResult.Object_Id     := new Object_Id'(P.Object_Id.all);
      TResult.Components    := Deep_Copy (P.Components);
      TResult.Mechanisms    := Deep_Copy (P.Mechanisms);

      return Result;
   end Duplicate_Profile;

   --------------------------------
   -- Marshall_DIOP_Profile_Body --
   --------------------------------

   procedure Marshall_DIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access)
   is
   begin
      Common_Marshall_Profile_Body
        (Buf,
         Profile,
         Address_Of
         (DIOP_Transport_Mechanism
          (Element (DIOP_Profile_Type (Profile.all).Mechanisms, 0).all.all)),
         True);
   end Marshall_DIOP_Profile_Body;

   ----------------------------------
   -- Unmarshall_DIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_DIOP_Profile_Body
     (Buffer : access Buffer_Type) return Profile_Access
   is
      Result  : constant Profile_Access := new DIOP_Profile_Type;
      Address : constant Utils.Sockets.Socket_Name :=
        Common_Unmarshall_Profile_Body
          (Buffer,
           Result,
           Unmarshall_Object_Id         => True,
           Unmarshall_Tagged_Components => False);
   begin
      --  Create transport mechanism

      Append
        (DIOP_Profile_Type (Result.all).Mechanisms,
         Create_Transport_Mechanism (Address));

      return Result;
   end Unmarshall_DIOP_Profile_Body;

   -----------
   -- Image --
   -----------

   overriding function Image (Prof : DIOP_Profile_Type) return String is
   begin
      return "Address : "
        & Utils.Sockets.Image
        (Address_Of
         (DIOP_Transport_Mechanism (Element (Prof.Mechanisms, 0).all.all)))
        & ", Object_Id : "
        & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
   begin
      pragma Debug (C, O ("DIOP Profile to corbaloc"));
      return
        Common_IIOP_DIOP_Profile_To_Corbaloc
        (P,
         Address_Of
         (DIOP_Transport_Mechanism
          (Element (DIOP_Profile_Type (P.all).Mechanisms, 0).all.all)),
         DIOP_Corbaloc_Prefix);
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      Result  : aliased Profile_Access := new DIOP_Profile_Type;
      Address : constant Utils.Sockets.Socket_Name :=
        Common_IIOP_DIOP_Corbaloc_To_Profile
          (Str,
           DIOP_Version_Major, DIOP_Version_Minor,
           Result'Access);
   begin
      --  Create transport mechanism

      Append
        (DIOP_Profile_Type (Result.all).Mechanisms,
         Create_Transport_Mechanism (Address));

      return Result;
   end Corbaloc_To_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Preference_Offset : constant String
        := PolyORB.Parameters.Get_Conf
        (Section => "diop",
         Key     => "polyorb.binding_data.diop.preference",
         Default => "0");

   begin
      --  XXX we impose a slight preference penalty to DIOP to favor IIOP
      --  by default. See F501-004.

      Preference := Preference_Default - 1 + Profile_Preference'Value
        (Preference_Offset);
      Register
       (Tag_DIOP,
        Marshall_DIOP_Profile_Body'Access,
        Unmarshall_DIOP_Profile_Body'Access);
      Register
        (Tag_DIOP,
         DIOP_Corbaloc_Prefix,
         Profile_To_Corbaloc'Access,
         Corbaloc_To_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.diop",
       Conflicts => Empty,
       Depends   => +"sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.GIOP.DIOP;
