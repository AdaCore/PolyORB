------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . I I O P             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  Binding data concrete implementation for IIOP.

with Ada.Streams;

with PolyORB.Configuration;
with PolyORB.Filters;
with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Protocols.GIOP;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Representations.CDR;
with PolyORB.References.IOR;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.IIOP is

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.IOR;
   use PolyORB.Representations.CDR;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Types;
   use PolyORB.GIOP_P.Tagged_Components;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_data.iiop");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for IIOP profiles.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (P : in out IIOP_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (P : in out IIOP_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (P : in out IIOP_Profile_Type) is
   begin
      Free (P.Object_Id);
      Release_Contents (P.Components);
   end Finalize;

   ------------------
   -- Bind_Profile --
   ------------------

   function Bind_Profile
     (Profile : IIOP_Profile_Type;
      The_ORB : Components.Component_Access)
     return Components.Component_Access
   is
      use PolyORB.ORB;
      use PolyORB.Components;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP;
      use PolyORB.Protocols.GIOP.IIOP;
      use PolyORB.Sockets;
      use PolyORB.Filters;
      use PolyORB.Filters.Slicers;

      Sock        : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      TE          : constant Transport.Transport_Endpoint_Access :=
        new Socket_Endpoint;
      Pro         : aliased IIOP_Protocol;
      Sli         : aliased Slicer_Factory;
      Prof        : constant Profile_Access := new IIOP_Profile_Type;
      --  This Profile_Access is stored in the created
      --  GIOP_Session, and free'd when the session is finalized.

      Filter : Filters.Filter_Access;

      TProf : IIOP_Profile_Type
        renames IIOP_Profile_Type (Prof.all);

   begin
      pragma Debug (O ("Bind IIOP profile: enter"));

      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);
      Create (Socket_Endpoint (TE.all), Sock);

      Chain_Factories ((0 => Sli'Unchecked_Access,
                        1 => Pro'Unchecked_Access));

      Filter :=
        Slicers.Create_Filter_Chain (Sli'Unchecked_Access);
      --  Filter must be an access to the lowest filter in
      --  the stack (the slicer in the case of GIOP).
      --  The call to CFC is qualified to work around a bug in
      --  the APEX compiler.

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
         S : GIOP_Session
           renames GIOP_Session (Upper (Filter).all);

      begin
         pragma Debug (O ("Bind IIOP profile: leave"));
         return S'Access;
      end;
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : IIOP_Profile_Type)
     return Profile_Tag
   is
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);

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
     (PF  : out IIOP_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

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
      Result : constant Profile_Access
        := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);
   begin
      TResult.Object_Id  := new Object_Id'(Oid);
      TResult.Address    := PF.Address;
      TResult.Components := Null_Tagged_Component_List;
      return Result;
   end Create_Profile;

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
      use PolyORB.Utils.Sockets;

      IIOP_Profile : IIOP_Profile_Type renames IIOP_Profile_Type (Profile.all);
      Profile_Body : Buffer_Access := new Buffer_Type;
   begin
      pragma Debug (O ("Marshall_IIOP_Profile_body: enter"));

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation

      Start_Encapsulation (Profile_Body);

      --  Marshalling the version

      Marshall (Profile_Body, IIOP_Profile.Major_Version);
      Marshall (Profile_Body, IIOP_Profile.Minor_Version);

      pragma Debug
        (O ("  Version = " & IIOP_Profile.Major_Version'Img & "."
            & IIOP_Profile.Minor_Version'Img));

      --  Marshalling the socket

      Marshall_Socket (Profile_Body, IIOP_Profile.Address);
      pragma Debug (O ("  Address = " & Sockets.Image (IIOP_Profile.Address)));

      --  Marshalling the object id

      Marshall
        (Profile_Body, Stream_Element_Array
         (IIOP_Profile.Object_Id.all));

      --  Marshalling the tagged components

      Marshall_Tagged_Component (Profile_Body, IIOP_Profile.Components);

      --  Marshalling the Profile_Body into IOR

      Marshall (Buf, Encapsulate (Profile_Body));
      Release (Profile_Body);

      pragma Debug (O ("Marshall_IIOP_Profile_body: leave"));

   end Marshall_IIOP_Profile_Body;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer       : access Buffer_Type)
     return Profile_Access
   is
      use PolyORB.Utils.Sockets;

      Result  : constant Profile_Access := new IIOP_Profile_Type;
      TResult : IIOP_Profile_Type renames IIOP_Profile_Type (Result.all);

      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access := new Buffers.Buffer_Type;

   begin
      pragma Debug (O ("Unmarshall_IIOP_Profile_body: enter"));

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation

      Decapsulate (Profile_Body'Access, Profile_Buffer);

      --  Unmarshalling the version

      TResult.Major_Version := Unmarshall (Profile_Buffer);
      TResult.Minor_Version := Unmarshall (Profile_Buffer);

      pragma Debug
        (O ("  Version = " & TResult.Major_Version'Img & "."
            & TResult.Minor_Version'Img));

      --  Unmarshalling the socket

      Unmarshall_Socket (Profile_Buffer, TResult.Address);

      pragma Debug (O ("  Address = " & Sockets.Image (TResult.Address)));

      --  Unarshalling the object id

      declare
         Str : aliased constant Stream_Element_Array :=
           Unmarshall (Profile_Buffer);
      begin
         TResult.Object_Id := new Object_Id'(Object_Id (Str));
         if TResult.Minor_Version /= 0 then
            TResult.Components := Unmarshall_Tagged_Component
              (Profile_Buffer);
         end if;
      end;
      Release (Profile_Buffer);

      pragma Debug (O ("Unmarshall_IIOP_Profile_body: leave"));

      return Result;
   end Unmarshall_IIOP_Profile_Body;

   -----------
   -- Image --
   -----------

   function Image
     (Prof : IIOP_Profile_Type)
     return String
   is
      use PolyORB.Sockets;

   begin
      return "Address : "
        & Image (Prof.Address)
        & ", Object_Id : "
        & PolyORB.Objects.Image (Prof.Object_Id.all);
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
         Key     => "polyorb.binding_data.iiop.preference",
         Default => "0");

   begin
      Preference := Preference_Default + Profile_Preference'Value
        (Preference_Offset);
      Register
       (Tag_Internet_IOP,
        Marshall_IIOP_Profile_Body'Access,
        Unmarshall_IIOP_Profile_Body'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.iiop",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Init      => Initialize'Access));

end PolyORB.Binding_Data.IIOP;
