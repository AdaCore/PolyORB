------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . D I O P        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  Binding data concrete implementation for DIOP.

with PolyORB.Binding_Data.GIOP.INET;
with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.DIOP;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Transport.Datagram.Sockets_In;
with PolyORB.Transport.Datagram.Sockets_Out;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.GIOP.DIOP is

   use PolyORB.Binding_Data.GIOP.INET;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.IOR;
   use PolyORB.References.Corbaloc;
   use PolyORB.Transport.Datagram;
   use PolyORB.Transport.Datagram.Sockets_In;
   use PolyORB.Transport.Datagram.Sockets_Out;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.binding_data.giop.diop");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for DIOP profiles.

   ------------------
   -- Bind_Profile --
   ------------------

   --  Factories

   Pro : aliased PolyORB.Protocols.GIOP.DIOP.DIOP_Protocol;
   DIOP_Factories : constant Filters.Factory_Array
     := (0 => Pro'Access);

   procedure Bind_Profile
     (Profile :     DIOP_Profile_Type;
      The_ORB :     Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Exceptions.Error_Container)
   is
      use PolyORB.Components;
      use PolyORB.Exceptions;
      use PolyORB.Filters;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP;
      use PolyORB.Protocols.GIOP.DIOP;
      use PolyORB.ORB;
      use PolyORB.Sockets;

      Sock        : Socket_Type;
      Remote_Addr : constant Sock_Addr_Type := Profile.Address;
      TE          : constant Transport.Transport_Endpoint_Access :=
        new Socket_Out_Endpoint;

   begin
      pragma Debug (O ("Bind DIOP profile: enter"));

      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode => Socket_Datagram);

      Create (Socket_Out_Endpoint (TE.all), Sock, Remote_Addr);
      Set_Allocation_Class (TE.all, Dynamic);

      Binding_Objects.Setup_Binding_Object
        (ORB.ORB_Access (The_ORB),
         TE,
         DIOP_Factories,
         ORB.Client,
         BO_Ref);

      pragma Debug (O ("Bind DIOP profile: leave"));

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag (Profile : DIOP_Profile_Type) return Profile_Tag is
      pragma Unreferenced (Profile);

   begin
      return Tag_DIOP;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
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

   procedure Create_Factory
     (PF  : out DIOP_Profile_Factory;
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
     (PF  : access DIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
   is
      Result : constant Profile_Access
        := new DIOP_Profile_Type;

      TResult : DIOP_Profile_Type
        renames DIOP_Profile_Type (Result.all);
   begin
      TResult.Version_Major := DIOP_Version_Major;
      TResult.Version_Minor := DIOP_Version_Minor;
      TResult.Object_Id     := new Object_Id'(Oid);
      TResult.Address       := PF.Address;
      TResult.Components    := Null_Tagged_Component_List;
      return Result;
   end Create_Profile;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access DIOP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean
   is
      use type PolyORB.Sockets.Sock_Addr_Type;

   begin
      return P.all in DIOP_Profile_Type
        and then DIOP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   --------------------------------
   -- Marshall_DIOP_Profile_Body --
   --------------------------------

   procedure Marshall_DIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access)
   is
   begin
      Common_Marshall_Profile_Body
        (Buf, Profile, DIOP_Profile_Type (Profile.all).Address, True);
   end Marshall_DIOP_Profile_Body;

   ----------------------------------
   -- Unmarshall_DIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_DIOP_Profile_Body
     (Buffer : access Buffer_Type)
      return Profile_Access
   is
      Result  : constant Profile_Access := new DIOP_Profile_Type;

   begin
      Common_Unmarshall_Profile_Body
        (Buffer, Result, DIOP_Profile_Type (Result.all).Address, True, False);

      return Result;
   end Unmarshall_DIOP_Profile_Body;

   -----------
   -- Image --
   -----------

   function Image (Prof : DIOP_Profile_Type) return String is
   begin
      return "Address : "
        & PolyORB.Sockets.Image (Prof.Address)
        & ", Object_Id : "
        & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return Types.String is
   begin
      pragma Debug (O ("DIOP Profile to corbaloc"));
      return Common_IIOP_DIOP_Profile_To_Corbaloc
        (P, DIOP_Profile_Type (P.all).Address, DIOP_Corbaloc_Prefix);
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : Types.String) return Profile_Access is
      Len : constant Integer := Length (DIOP_Corbaloc_Prefix);
   begin
      if Length (Str) > Len
        and then To_String (Str) (1 .. Len) = DIOP_Corbaloc_Prefix
      then
         declare
            Result : Profile_Access := new DIOP_Profile_Type;
         begin
            Common_IIOP_DIOP_Corbaloc_To_Profile
              (Str, Len, Result, DIOP_Profile_Type (Result.all).Address);
            return Result;
         end;
      end if;

      return null;
   end Corbaloc_To_Profile;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : DIOP_Profile_Type)
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
        (Section => "diop",
         Key     => "polyorb.binding_data.diop.preference",
         Default => "0");

   begin
      Preference := Preference_Default + Profile_Preference'Value
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
       Init      => Initialize'Access));
end PolyORB.Binding_Data.GIOP.DIOP;
