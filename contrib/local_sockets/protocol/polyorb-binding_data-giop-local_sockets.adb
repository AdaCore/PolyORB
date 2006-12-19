------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.BINDING_DATA.GIOP.LOCAL_SOCKETS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with Ada.Streams;

with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.Local_Sockets;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Transport.Connected.Local_Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Representations.CDR.Common;

package body PolyORB.Binding_Data.GIOP.Local_Sockets is

   use Ada.Streams;

   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.IOR;
   use PolyORB.Transport.Connected.Local_Sockets;
   use PolyORB.Types;
   use PolyORB.Representations.CDR.Common;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_data.giop.ls");

   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for LS profiles.

   ------------------
   -- Bind_Profile --
   ------------------

   --  Factories

   Pro          : aliased
     PolyORB.Protocols.GIOP.Local_Sockets.Local_Sockets_Protocol;
   LS_Factories : constant Filters.Factory_Array := (0 => Pro'Access);

   procedure Bind_Profile
     (Profile : LS_Profile_Type;
      The_ORB : Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
      pragma Unreferenced (Error);

      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.Filters;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP;
      use PolyORB.Protocols.GIOP.Local_Sockets;
      use PolyORB.ORB;

      TE : constant Transport.Transport_Endpoint_Access :=
         new Local_Socket_Endpoint;
      S : Local_Socket_Type;
   begin
      pragma Debug (O ("Bind LS profile: enter"));

      Set_Address (S, Profile.Address);
      Create (Local_Socket_Endpoint (TE.all), S);
      Set_Allocation_Class (TE.all, Dynamic);

      Binding_Objects.Setup_Binding_Object
        (ORB.ORB_Access (The_ORB),
         TE,
         LS_Factories,
         ORB.Client,
         BO_Ref);

      pragma Debug (O ("Bind LS profile: leave"));
   end Bind_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile (P : LS_Profile_Type) return Profile_Access is
      use PolyORB.Objects;

      Result : constant Profile_Access := new LS_Profile_Type;

      TResult : LS_Profile_Type renames LS_Profile_Type (Result.all);

      PP : LS_Profile_Type renames P;

   begin
      TResult.Version_Major := PP.Version_Major;
      TResult.Version_Minor := PP.Version_Minor;
      TResult.Object_Id     := new Object_Id'(PP.Object_Id.all);
      TResult.Address       := PP.Address;
      TResult.Components    := Deep_Copy (PP.Components);

      return Result;
   end Duplicate_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag (Profile : LS_Profile_Type) return Profile_Tag is
      pragma Unreferenced (Profile);

   begin
      return Tag_Internet_IOP;
      --  XXX ???
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : LS_Profile_Type)
      return    Profile_Preference
   is
      pragma Unreferenced (Profile);

   begin
      return Preference;
   end Get_Profile_Preference;

   --------------------
   -- Create_Factory --
   --------------------

   procedure Create_Factory
     (PF  : out LS_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access)
   is
      pragma Unreferenced (ORB);

   begin
      pragma Debug (O ("TAP :" & Image (Local_Socket_Access_Point (TAP.all))));

      PF.Address := Address_Of (Local_Socket_Access_Point (TAP.all));
      pragma Debug (O ("Create factory :" & PF.Address.LPort'Img));
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (PF   : access LS_Profile_Factory;
      Oid  : Objects.Object_Id)
      return Profile_Access
   is
      Result : constant Profile_Access := new LS_Profile_Type;

      TResult : LS_Profile_Type renames LS_Profile_Type (Result.all);
   begin
      pragma Debug (O ("Create_Profile: enter"));
      TResult.Version_Major := LS_Version_Major;
      TResult.Version_Minor := LS_Version_Minor;
      TResult.Object_Id     := new Object_Id'(Oid);
      TResult.Address       := PF.Address;
      TResult.Components    := Null_Tagged_Component_List;

      pragma Debug (O ("Create_Profile: leave"));

      return Result;
   end Create_Profile;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF   : access LS_Profile_Factory;
      P    : access Profile_Type'Class)
      return Boolean
   is
   begin
      return P.all in LS_Profile_Type
        and then LS_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   ------------------------------
   -- Marshall_LS_Profile_Body --
   ------------------------------

   procedure Marshall_LS_Profile_Body
     (Buf     : access Buffer_Type;
      Profile : Profile_Access)
   is
      GIOP_Profile : GIOP_Profile_Type'Class renames
        GIOP_Profile_Type'Class (Profile.all);
      Profile_Body : Buffer_Access := new Buffer_Type;

   begin
      pragma Debug (O ("Common_Marshall_Profile_Body: enter"));

      --  A Profile Body is an encapsulation

      Start_Encapsulation (Profile_Body);

      --  Version

      Marshall (Profile_Body, GIOP_Profile.Version_Major);
      Marshall (Profile_Body, GIOP_Profile.Version_Minor);

      pragma Debug
        (O ("  Version = " &
            GIOP_Profile.Version_Major'Img &
            "." &
            GIOP_Profile.Version_Minor'Img));

      --  Marshalling of a Socket

      Marshall_Latin_1_String
        (Profile_Body,
         PolyORB.Types.To_PolyORB_String
            (LS_Profile_Type (Profile.all).Address.LPort'Img));

      pragma Debug
        (O ("  Address = " &
             LS_Profile_Type (Profile.all).Address.LPort'Img));

      --  Marshalling the object id

      Marshall
        (Profile_Body,
         Stream_Element_Array (GIOP_Profile.Object_Id.all));

      --  Marshalling the tagged components

      Marshall_Tagged_Component (Profile_Body, GIOP_Profile.Components);

      --  Marshalling the Profile_Body into IOR

      Marshall (Buf, Encapsulate (Profile_Body));
      Release (Profile_Body);

      pragma Debug (O ("Common_Marshall_Profile_Body: leave"));
   end Marshall_LS_Profile_Body;

   -------------------------------------
   -- Unmarshall_LS_Profile_Body --
   -------------------------------------

   function Unmarshall_LS_Profile_Body
     (Buffer : access Buffer_Type)
      return   Profile_Access
   is
      Result  : constant Profile_Access := new LS_Profile_Type;
      TResult : GIOP_Profile_Type'Class renames
        GIOP_Profile_Type'Class (Result.all);

      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access         := new Buffers.Buffer_Type;

   begin
      pragma Debug (O ("Common_Unmarshall_Profile_Body: enter"));

      --  A Profile Body is an encapsulation

      Decapsulate (Profile_Body'Access, Profile_Buffer);

      TResult.Version_Major := Unmarshall (Profile_Buffer);
      TResult.Version_Minor := Unmarshall (Profile_Buffer);

      pragma Debug
        (O ("  Version = " &
             TResult.Version_Major'Img &
             "." &
             TResult.Version_Minor'Img));

      --  Unmarshalling the socket

      declare
         LPort_Image : constant Types.String :=
            Unmarshall_Latin_1_String (Profile_Buffer);
      begin
         pragma Debug (O ("  Address = " & To_String (LPort_Image)));

         LS_Profile_Type (Result.all).Address.LPort :=
            Port'Value (To_String (LPort_Image));
      end;

      --  Unmarshalling the object id

      declare
         Str : aliased constant Stream_Element_Array :=
            Unmarshall (Profile_Buffer);
      begin
         TResult.Object_Id := new Object_Id'(Object_Id (Str));
      end;

      --  Unmarshalling tagged components

      TResult.Components := Unmarshall_Tagged_Component (Profile_Buffer);

      Release (Profile_Buffer);

      pragma Debug (O ("Unmarshall_LS_Profile_body: leave"));

      return Result;
   end Unmarshall_LS_Profile_Body;

   -----------
   -- Image --
   -----------

   function Image (Prof : LS_Profile_Type) return String is
   begin
      return "LS profile: Address = " &
             Prof.Address.LPort'Img &
             ", Object_Id : " &
             PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Prof : LS_Profile_Factory) return String is
   begin
      return "LS profile Factory : Address = " & Prof.Address.LPort'Img;
   end Image;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : LS_Profile_Type)
      return    PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);

   begin
      return PolyORB.Smart_Pointers.Entity_Ptr (PolyORB.ORB.Object_Adapter
                                                   (PolyORB.Setup.The_ORB));
   end Get_OA;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Preference_Offset : constant String :=
         PolyORB.Parameters.Get_Conf
           (Section => "lsiop",
            Key     => "polyorb.binding_data.lsiop.preference",
            Default => "0");
   begin
      Preference := Preference_Default +
                    Profile_Preference'Value (Preference_Offset);
      Register
        (Tag_Internet_IOP,
         Marshall_LS_Profile_Body'Access,
         Unmarshall_LS_Profile_Body'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
     (Name      => +"binding_data.lsiop",
      Conflicts => Empty,
      Depends   => +"local_sockets",
      Provides  => +"binding_factories",
      Implicit  => False,
      Init      => Initialize'Access,
      Shutdown  => null));
end PolyORB.Binding_Data.GIOP.Local_Sockets;
