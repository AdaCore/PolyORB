------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I I O P        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

--  Binding data concrete implementation for IIOP.

with PolyORB.Binding_Data.GIOP.INET;
with PolyORB.Binding_Objects;
with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.GIOP.IIOP is

   use PolyORB.Binding_Data.GIOP.INET;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.Corbaloc;
   use PolyORB.References.IOR;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.giop.iiop");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   IIOP_Corbaloc_Prefix : constant String := "iiop";

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for IIOP profiles.

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   ------------------
   -- Bind_Profile --
   ------------------

   --  Factories

   Sli            : aliased PolyORB.Filters.Slicers.Slicer_Factory;
   Pro            : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : constant PolyORB.Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   procedure Bind_Profile
     (Profile :     IIOP_Profile_Type;
      The_ORB :     Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.Filters;
      use PolyORB.ORB;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP;
      use PolyORB.Protocols.GIOP.IIOP;
      use PolyORB.Sockets;

      Sock        : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      TE          : constant Transport.Transport_Endpoint_Access :=
        new Socket_Endpoint;

   begin
      pragma Debug (O ("Bind IIOP profile: enter"));

      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);
      Create (Socket_Endpoint (TE.all), Sock);
      Set_Allocation_Class (TE.all, Dynamic);

      Binding_Objects.Setup_Binding_Object
        (ORB.ORB_Access (The_ORB),
         TE,
         IIOP_Factories,
         ORB.Client,
         BO_Ref);

      pragma Debug (O ("Bind IIOP profile: leave"));

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag (Profile : IIOP_Profile_Type) return Profile_Tag is
      pragma Unreferenced (Profile);

   begin
      return Tag_Internet_IOP;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : IIOP_Profile_Type)
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
     (PF  : out IIOP_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Unreferenced (ORB);

   begin
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
   is
      use PolyORB.GIOP_P.Tagged_Components;

      Result : constant Profile_Access := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type renames IIOP_Profile_Type (Result.all);

   begin
      TResult.Version_Major := IIOP_Version_Major;
      TResult.Version_Minor := IIOP_Version_Minor;
      TResult.Object_Id     := new Object_Id'(Oid);
      TResult.Address       := PF.Address;

      --  Fetch tagged components for Oid

      TResult.Components := Fetch_Components (TResult.Object_Id);

      return Result;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile
     (P : IIOP_Profile_Type)
     return Profile_Access
   is
      use PolyORB.Objects;

      Result : constant Profile_Access := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);

      PP : IIOP_Profile_Type renames P;

   begin
      TResult.Version_Major := PP.Version_Major;
      TResult.Version_Minor := PP.Version_Minor;
      TResult.Object_Id     := new Object_Id'(PP.Object_Id.all);
      TResult.Address       := PP.Address;
      TResult.Components    := Deep_Copy (PP.Components);

      return Result;
   end Duplicate_Profile;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access IIOP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean
   is
      use type PolyORB.Sockets.Sock_Addr_Type;

   begin
      return P.all in IIOP_Profile_Type
        and then IIOP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Marshall_IIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access)
   is
   begin
      Common_Marshall_Profile_Body
        (Buf, Profile, IIOP_Profile_Type (Profile.all).Address, True);
   end Marshall_IIOP_Profile_Body;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer : access Buffer_Type)
      return Profile_Access
   is
      Result : constant Profile_Access := new IIOP_Profile_Type;

   begin
      Common_Unmarshall_Profile_Body
        (Buffer, Result, IIOP_Profile_Type (Result.all).Address, True, False);

      return Result;
   end Unmarshall_IIOP_Profile_Body;

   -----------
   -- Image --
   -----------

   function Image (Prof : IIOP_Profile_Type) return String is
   begin
      return "Address : "
        & PolyORB.Sockets.Image (Prof.Address)
        & ", Object_Id : "
        & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
   begin
      pragma Debug (O ("IIOP Profile to corbaloc"));
      return Common_IIOP_DIOP_Profile_To_Corbaloc
        (P, IIOP_Profile_Type (P.all).Address, IIOP_Corbaloc_Prefix);
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      Result : Profile_Access := new IIOP_Profile_Type;
   begin
      Common_IIOP_DIOP_Corbaloc_To_Profile
        (Str, IIOP_Version_Major, IIOP_Version_Minor, Result,
         IIOP_Profile_Type (Result.all).Address);
      return Result;
   end Corbaloc_To_Profile;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : IIOP_Profile_Type)
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
        (Section => "iiop",
         Key     => "polyorb.binding_data.iiop.preference",
         Default => "0");

   begin
      Preference := Preference_Default + Profile_Preference'Value
        (Preference_Offset);
      Register
       (Tag_Internet_IOP,
        Marshall_IIOP_Profile_Body'Access,
        Unmarshall_IIOP_Profile_Body'Access);
      Register
        (Tag_Internet_IOP,
         IIOP_Corbaloc_Prefix,
         Profile_To_Corbaloc'Access,
         Corbaloc_To_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.iiop",
       Conflicts => Empty,
       Depends   => +"protocols.giop.iiop" & "sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Binding_Data.GIOP.IIOP;
