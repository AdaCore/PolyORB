------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . I I O P             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Example binding data concrete implementation.

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Configuration;
with PolyORB.Filters;
with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Protocols;
with PolyORB.Protocols.GIOP;
with PolyORB.Representations.CDR;
with PolyORB.References.IOR;
with PolyORB.Transport.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.IIOP is

   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.IOR;
   use PolyORB.Representations.CDR;
   use PolyORB.Transport.Sockets;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_data.iiop");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Marshall_Socket
        (Buffer   : access Buffer_Type;
         Sock     : Sockets.Sock_Addr_Type);

   procedure Unmarshall_Socket
    (Buffer   : access Buffer_Type;
     Sock     : out Sockets.Sock_Addr_Type);

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for IIOP profiles.

   procedure Initialize is
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

   procedure Initialize (P : in out IIOP_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out IIOP_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out IIOP_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   procedure Bind_Non_Local_Profile
     (Profile : IIOP_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Filter  : out Components.Component_Access)
   is
      use PolyORB.Components;
      use PolyORB.Protocols;
      use PolyORB.Protocols.GIOP;
      use PolyORB.Sockets;
      use PolyORB.Filters;
      use PolyORB.Filters.Slicers;

      Sock : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      Pro  : aliased GIOP_Protocol;
      Sli  : aliased Slicer_Factory;
      Prof : constant Profile_Access := new IIOP_Profile_Type;
      --  This Profile_Access is stored in the created
      --  GIOP_Session, and free'd when the session is finalised.

      TProf : IIOP_Profile_Type
        renames IIOP_Profile_Type (Prof.all);

   begin
      pragma Debug (O ("Bind IIOP profile: enter"));
      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);
      TE := new Transport.Sockets.Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), Sock);

      Chain_Factories ((0 => Sli'Unchecked_Access,
                        1 => Pro'Unchecked_Access));

      Filter := Component_Access
        (Slicers.Create_Filter_Chain (Sli'Unchecked_Access));
      --  Filter must be an access to the lowest filter in
      --  the stack (the slicer in the case of GIOP).
      --  The call to CFC is qualified to work around a bug in
      --  the APEX compiler.

      pragma Debug (O ("Preparing local copy of profile"));
      TProf.Address := Profile.Address;
      TProf.Object_Id := Profile.Object_Id;
      Adjust (TProf);
      pragma Debug (O ("Adjusted local copy of profile"));

      declare
         S : GIOP_Session
           renames GIOP_Session
           (Upper (Filter_Access (Filter)).all);
      begin
         Store_Profile (S'Access, Prof);
         Set_Version
           (S'Access,
            Profile.Major_Version,
            Profile.Minor_Version);
      end;

      --  The caller will invoke Register_Endpoint on TE.

      pragma Debug (O ("Bind IIOP profile: leave"));
   end Bind_Non_Local_Profile;

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

   procedure Create_Factory
     (PF  : out IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
   begin
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      Oid : Objects.Object_Id)
     return Profile_Access
   is
      use PolyORB.Transport.Sockets;
      use Component_Seq;

      Result : constant Profile_Access
        := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(Oid);
      TResult.Address   := PF.Address;
      TResult.Components := Null_Sequence;
      return  Result;
   end Create_Profile;

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
      Profile : Profile_Access)
   is

      IIOP_Profile : IIOP_Profile_Type renames IIOP_Profile_Type (Profile.all);
      Profile_Body : Buffer_Access := new Buffer_Type;

   begin

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.
      Start_Encapsulation (Profile_Body);

      --  Version
      Marshall (Profile_Body, IIOP_Major_Version);
      Marshall (Profile_Body, IIOP_Minor_Version);

      --  Marshalling of a Socket
      Marshall_Socket (Profile_Body, IIOP_Profile.Address);

      --  Marshalling of the Object Id
      Marshall
        (Profile_Body, Stream_Element_Array
         (IIOP_Profile.Object_Id.all));

      --  Marshall the tagged components (none for now).
      Marshall (Profile_Body, Types.Unsigned_Long'(0));

      --  Marshall the Profile_Body into IOR.
      Marshall (Buf, Encapsulate (Profile_Body));
      Release (Profile_Body);

   end Marshall_IIOP_Profile_Body;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer       : access Buffer_Type)
     return Profile_Access
   is
      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access := new Buffers.Buffer_Type;
      --  Length         : CORBA.Long;
      Result         : constant Profile_Access := new IIOP_Profile_Type;
      TResult        : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);


   begin
      pragma Debug (O ("Unmarshall_IIOP_Profile_body: enter"));
      Decapsulate (Profile_Body'Access, Profile_Buffer);

      TResult.Major_Version  := Unmarshall (Profile_Buffer);
      TResult.Minor_Version  := Unmarshall (Profile_Buffer);
      pragma Debug
        (O ("  Version = " & TResult.Major_Version'Img & "."
            & TResult.Minor_Version'Img));

      Unmarshall_Socket (Profile_Buffer, TResult.Address);
      pragma Debug (O ("  Address = " & Sockets.Image (TResult.Address)));
      declare
         Str  : aliased constant Stream_Element_Array :=
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

   procedure Marshall_Socket
     (Buffer : access Buffer_Type;
      Sock   : Sockets.Sock_Addr_Type)
   is
      use PolyORB.Sockets;

      Str  : constant Types.String := To_PolyORB_String (Image (Sock.Addr));
   begin

      --  Marshalling of the Host as a string
      Marshall (Buffer, Str);

      --  Marshalling of the port
      Marshall (Buffer, Types.Unsigned_Short (Sock.Port));

   end Marshall_Socket;


   procedure Unmarshall_Socket
    (Buffer : access Buffer_Type;
     Sock   : out Sockets.Sock_Addr_Type)
   is
      use PolyORB.Sockets;

      Addr_Image : constant Standard.String
        := PolyORB.Types.To_Standard_String
        (PolyORB.Types.String'(Unmarshall (Buffer)));
      Port : Types.Unsigned_Short;
   begin

      --  Unmarshalling of the Host
      Sock.Addr := Inet_Addr (Addr_Image);

      --  Unmarshalling of the port
      Port := Unmarshall (Buffer);
      Sock.Port := Port_Type (Port);

   end Unmarshall_Socket;


   procedure Marshall_Tagged_Component
     (Buffer         : access Buffer_Type;
      Components     : Component_Seq.Sequence)

   is
      use Component_Seq;
   begin

      Marshall (Buffer,  Types.Unsigned_Long (Length (Components)));
      for I in 1 .. Length (Components) loop
         Marshall (Buffer, Element_Of (Components, I).Tag);
         Marshall (Buffer, Element_Of (Components, I).Component_Data.all);
      end loop;

   end Marshall_Tagged_Component;


   function  Unmarshall_Tagged_Component
     (Buffer   : access Buffer_Type)
     return Component_Seq.Sequence
   is
      use Component_Seq;
      Comp        : Tagged_Component;
      Components  : Component_Seq.Sequence := Null_Sequence;
      Len         : Types.Unsigned_Long;
   begin
      Len := Unmarshall (Buffer);
      for I in 1 .. Len loop
         Comp.Tag  := Unmarshall (Buffer);
         Comp.Component_Data := new Stream_Element_Array'(Unmarshall
                            (Buffer));
         Append (Components, Comp);
      end loop;
      return Components;
   end Unmarshall_Tagged_Component;

   -----------
   -- Image --
   -----------

   function Image (Prof : IIOP_Profile_Type) return String is
      use PolyORB.Sockets;
   begin
      return "Address : " & Image (Prof.Address) &
        ", Object_Id : " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"binding_data.iiop",
       Conflicts => Empty,
       Depends => Empty,
       Provides => Empty,
       Init => Initialize'Access));
end PolyORB.Binding_Data.IIOP;
