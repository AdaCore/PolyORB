------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . I I O P                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with CORBA; use CORBA;
with Broca.Exceptions;
with Broca.CDR; use Broca.CDR;
with Broca.Buffers;     use Broca.Buffers;
with Broca.Buffers.IO_Operations;
with Broca.IOP;         use Broca.IOP;
with Broca.Sequences;   use Broca.Sequences;
with Sockets.Constants;
with Sockets.Naming;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.IIOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iiop");
   procedure O is new Broca.Debug.Output (Flag);

   function Port_To_Network_Port (Port : CORBA.Unsigned_Short)
     return Interfaces.C.unsigned_short;

   procedure Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type;
      Profile  : out Profile_Ptr);

   procedure Marshall_IIOP_Profile_Body
     (IOR   : access Buffer_Type;
      Profile  : access Profile_Type'Class);

   --------------------
   -- Get_Profile_Tag --
   --------------------

   function Get_Profile_Tag
     (Profile : Profile_IIOP_Type)
     return Profile_Tag is
   begin
      return Tag_Internet_IOP;
   end Get_Profile_Tag;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   procedure Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type;
      Profile  : out Profile_Ptr)
   is
      Minor        : CORBA.Octet;
      Major        : CORBA.Octet;
      IIOP_Profile : Profile_IIOP_Ptr := new Profile_IIOP_Type;

      Profile_Body : aliased Encapsulation
        := Unmarshall (Buffer);
      Profile_Body_Buffer : aliased Buffer_Type;
   begin
      Decapsulate (Profile_Body'Access, Profile_Body_Buffer'Access);

      Major := Unmarshall (Profile_Body_Buffer'Access);
      Minor := Unmarshall (Profile_Body_Buffer'Access);

      IIOP_Profile.Host := Unmarshall (Profile_Body_Buffer'Access);
      IIOP_Profile.Port := Unmarshall (Profile_Body_Buffer'Access);
      IIOP_Profile.Network_Port := Port_To_Network_Port (IIOP_Profile.Port);
      IIOP_Profile.Object_Key := Unmarshall (Profile_Body_Buffer'Access);

      Profile :=  Profile_Ptr (IIOP_Profile);
   end Unmarshall_IIOP_Profile_Body;

   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Marshall_IIOP_Profile_Body
     (IOR     : access Buffer_Type;
      Profile : access Profile_Type'Class)
   is
      IIOP_Profile : Profile_IIOP_Type
        renames Profile_IIOP_Type (Profile.all);

      use Broca.CDR;

      Profile_Body : aliased Buffer_Type;
   begin

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.

      Start_Encapsulation (Profile_Body'Access);

      --  Version
      --  FIXME: Version should not be hard-coded.
      Marshall (Profile_Body'Access, CORBA.Octet'(1));
      Marshall (Profile_Body'Access, CORBA.Octet'(0));

      --  Host
      Marshall (Profile_Body'Access, IIOP_Profile.Host);

      --  Port
      Marshall (Profile_Body'Access, IIOP_Profile.Port);

      --  Object key
      Marshall (Profile_Body'Access, IIOP_Profile.Object_Key);

      --  Marshall the Profile_Body into IOR.
      Marshall (IOR, Encapsulate (Profile_Body'Access));
      Release (Profile_Body);
   end Marshall_IIOP_Profile_Body;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Profile_IIOP_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      return Profile.Object_Key;
   end Get_Object_Key;

   --------------------------
   -- Port_To_Network_Port --
   --------------------------

   function Port_To_Network_Port
     (Port : CORBA.Unsigned_Short)
     return Interfaces.C.unsigned_short is
   begin
      if Broca.Buffers.Host_Order = Little_Endian then
         return Interfaces.C.unsigned_short
           ((Port / 256) + (Port mod 256) * 256);
      else
         return Interfaces.C.unsigned_short (Port);
      end if;
   end Port_To_Network_Port;

   --------------------
   -- Create_Profile --
   --------------------

   --  FIXME: This contains duplicate functionality
   --    w.r.t. Unmarshall_IIOP_Profile_Body.
   --    ==> This must be rewritten in terms of
   --        Unmarshall_IIOP_Profile_Body.

   procedure Create_Profile
     (Buffer : access Buffer_Type;
      Profile : out Profile_Ptr)
   is
      use Broca.CDR;

      Profile_Body : aliased Encapsulation
        := Unmarshall (Buffer);
      Profile_Body_Buffer : aliased Buffer_Type;
      Res : Profile_IIOP_Ptr;
      Nbr_Seq : CORBA.Unsigned_Long;

   begin
      Res := new Profile_IIOP_Type;

      Decapsulate (Profile_Body'Access,
                   Profile_Body_Buffer'Access);

      --  Extract version
      Res.IIOP_Version.Major
        := Unmarshall (Profile_Body_Buffer'Access);
      Res.IIOP_Version.Minor
        := Unmarshall (Profile_Body_Buffer'Access);

      if Res.IIOP_Version.Major /= 1
        or else Res.IIOP_Version.Minor > 1
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      Res.Host := Unmarshall (Profile_Body_Buffer'Access);
      Res.Port := Unmarshall (Profile_Body_Buffer'Access);
      pragma Debug (O ("host: " & To_String (Res.Host)));
      pragma Debug (O ("port: " & CORBA.Unsigned_Short'Image (Res.Port)));
      Res.Network_Port := Port_To_Network_Port (Res.Port);
      Res.Object_Key := Broca.Sequences.Unmarshall
        (Profile_Body_Buffer'Access);

      if Res.IIOP_Version.Minor = 1 then
         Nbr_Seq := Unmarshall (Profile_Body_Buffer'Access);
         if Nbr_Seq /= 0 then
            --  FIXME: Multiple components are not yet handled.
            Broca.Exceptions.Raise_Bad_Param;
         end if;
      end if;

      --  Store profile result.
      Profile := Res.all'Access;
   end Create_Profile;

   procedure Create_Socket_Address (Profile : in out Profile_IIOP_Type);

   procedure Create_Socket_Address (Profile : in out Profile_IIOP_Type)
   is
      use Ada.Strings.Unbounded;
      use Sockets.Naming;
      use Sockets.Constants;
      use Sockets.Thin;
   begin
      --  This object was never connected.
      --  Create a connection now.
      Profile.Socket_Address.Sin_Family := Af_Inet;
      Profile.Socket_Address.Sin_Port := Profile.Network_Port;
      Profile.Socket_Address.Sin_Addr :=
        To_In_Addr
        (Address_Of (To_String (Unbounded_String (Profile.Host))));
   end Create_Socket_Address;

   function Create_Strand return Strand_Ptr;

   function Create_Strand return Strand_Ptr is
      Res : Strand_Ptr;
   begin
      Res := new Strand_Type;
      Res.Next := null;
      Res.Fd := Sockets.Thin.Failure;
      Res.Request_Id := 1;
      return Res;
   end Create_Strand;

   procedure Open_Strand
     (Profile : access Profile_IIOP_Type; Strand : Strand_Ptr);

   procedure Open_Strand (Profile : access Profile_IIOP_Type;
                          Strand : Strand_Ptr)
   is
      use Sockets.Naming;
      use Sockets.Constants;
      use Sockets.Thin;
      use Interfaces.C;
   begin
      Strand.Fd := C_Socket (Af_Inet, Sock_Stream, 0);
      if Strand.Fd = Failure then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;
      if C_Connect (Strand.Fd,
                    Profile.Socket_Address'Address,
                    Profile.Socket_Address'Size / 8) = Failure
      then
         C_Close (Strand.Fd);
         Strand.Fd := Failure;
         Broca.Exceptions.Raise_Comm_Failure;
      end if;
   end Open_Strand;

   type Strand_Connection_Type is new Connection_Type with
      record
         Strand : Strand_Ptr;
      end record;

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : access Buffer_Type);

   function Receive
     (Connection : access Strand_Connection_Type;
      Length     : Opaque.Index_Type)
     return Opaque.Octet_Array;

   procedure Release_Connection
     (Connection : access Strand_Connection_Type);

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : access Buffer_Type)
   is
      use Sockets.Thin;
      use Interfaces.C;
   begin
      --  Read (Buffer, Bytes);
      pragma Debug (O ("Dump outgoing buffer"));
      Broca.Buffers.Show (Buffer.all);
      begin
         Broca.Buffers.IO_Operations.Write_To_FD
           (Connection.Strand.Fd, Buffer);
      exception
         when others =>
            Broca.Exceptions.Raise_Comm_Failure;
      end;
      pragma Debug (O ("Message correctly sent"));
   end Send;

   -------------
   -- Receive --
   -------------

   function Receive
     (Connection : access Strand_Connection_Type;
      Length     : Opaque.Index_Type)
     return Opaque.Octet_Array
   is
      use Broca.Opaque;

      Result : Octet_Array (1 .. Length);
   begin
      if Length = 0 then
         pragma Debug (O ("Null length in Receive"));

         return Result;
      end if;

      declare
         use Sockets.Thin;
         use Interfaces.C;

         Received : Interfaces.C.int;
      begin
         Received := C_Recv
           (Connection.Strand.Fd,
            Result (1)'Address,
            Interfaces.C.int (Length), 0);

         if Received /=  Interfaces.C.int (Length) then
            Broca.Exceptions.Raise_Comm_Failure;
         else
            return Result;
         end if;

      end;
   end Receive;

   procedure Release_Connection (Connection : access Strand_Connection_Type) is
   begin
      Connection.Strand.Lock.Unlock;
   end Release_Connection;

   --  Find a free connection (or create a new one) for a message to an
   --  OBJECT via PROFILE.
   function Find_Connection
     (Profile : access Profile_IIOP_Type)
     return Connection_Ptr
   is
      use Interfaces.C;
      Strand : Strand_Ptr;
      Success : Boolean;
   begin
      Strand := Profile.Strands;
      if Strand = null then
         Create_Socket_Address (Profile.all);
      end if;
      while Strand /= null loop
         Strand.Lock.Trylock (Success);
         exit when Success;
         Strand := Strand.Next;
      end loop;
      if Strand = null then
         Strand := Create_Strand;
         Strand.Lock.Lock;
         Profile.Lock.Lock_W;
         Strand.Next := Profile.Strands;
         Profile.Strands := Strand;
         Profile.Lock.Unlock_W;
      end if;
      if Strand.Fd = Sockets.Thin.Failure then
         Open_Strand (Profile, Strand);
      end if;
      return new Strand_Connection_Type'(Connection_Type
                                         with Strand => Strand);
   end Find_Connection;

begin
   Register
     (Tag_Internet_IOP,
      Marshall_IIOP_Profile_Body'Access,
      Unmarshall_IIOP_Profile_Body'Access);
end Broca.IIOP;
