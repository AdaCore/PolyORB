------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . I I O P                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.20 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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
with Broca.Marshalling; use Broca.Marshalling;
with Broca.Buffers;     use Broca.Buffers;
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

   procedure Decapsulate_Profile
     (Buffer   : in out Buffer_Descriptor;
      Profile  : out Profile_Ptr);

   procedure Encapsulate_Profile
     (Buffer   : in out Buffer_Descriptor;
      From     : in Buffer_Index_Type;
      Profile  : in Profile_Ptr);

   --------------------
   -- Get_Profile_Id --
   --------------------

   function Get_Profile_Id
     (Profile : Profile_IIOP_Type)
     return Profile_Id is
   begin
      return Tag_Internet_IOP;
   end Get_Profile_Id;

   -------------------------
   -- Decapsulate_Profile --
   -------------------------

   procedure Decapsulate_Profile
     (Buffer   : in out Buffer_Descriptor;
      Profile  : out Profile_Ptr)
   is
      Old_Endian   : Boolean := Get_Endianess (Buffer);
      New_Endian   : Boolean;
      Minor        : Octet;
      Major        : Octet;
      IIOP_Profile : Profile_IIOP_Ptr := new Profile_IIOP_Type;
   begin
      Unmarshall (Buffer, New_Endian);
      Set_Endianess (Buffer, New_Endian);

      Unmarshall (Buffer, Major);
      Unmarshall (Buffer, Minor);

      Unmarshall (Buffer, IIOP_Profile.Host);
      Unmarshall (Buffer, IIOP_Profile.Port);
      IIOP_Profile.Network_Port := Port_To_Network_Port (IIOP_Profile.Port);
      Align_Size (Buffer, UL_Size);
      Unmarshall (Buffer, IIOP_Profile.Object_Key);
      Set_Endianess (Buffer, Old_Endian);

      Profile :=  Profile_Ptr (IIOP_Profile);
   end Decapsulate_Profile;

   -------------------------
   -- Encapsulate_Profile --
   -------------------------

   procedure Encapsulate_Profile
     (Buffer   : in out Buffer_Descriptor;
      From     : in Buffer_Index_Type;
      Profile  : in Profile_Ptr)
   is
      IIOP_Profile : Profile_IIOP_Type renames Profile_IIOP_Type (Profile.all);
   begin
      Rewind (Buffer);

      Skip_Bytes (Buffer, From);

      --  Endianess
      Compute_New_Size (Buffer, O_Size, O_Size);

      --  Major version
      Compute_New_Size (Buffer, O_Size, O_Size);

      --  Minor version
      Compute_New_Size (Buffer, O_Size, O_Size);

      --  Host
      Compute_New_Size (Buffer, IIOP_Profile.Host);

      --  Port
      Compute_New_Size (Buffer, US_Size, US_Size);

      Align_Size (Buffer, UL_Size);

      Compute_New_Size (Buffer, IIOP_Profile.Object_Key);

      Allocate_Buffer_And_Clear_Pos (Buffer, Full_Size (Buffer));

      Skip_Bytes (Buffer, From);

      Marshall (Buffer, Is_Little_Endian);
      Marshall (Buffer, CORBA.Octet'(1));
      Marshall (Buffer, CORBA.Octet'(0));
      Marshall (Buffer, IIOP_Profile.Host);
      Marshall (Buffer, IIOP_Profile.Port);
      Align_Size (Buffer, UL_Size);
      Marshall (Buffer, IIOP_Profile.Object_Key);
   end Encapsulate_Profile;

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
      if Broca.Buffers.Is_Little_Endian then
         return Interfaces.C.unsigned_short
           ((Port / 256) + (Port mod 256) * 256);
      else
         return Interfaces.C.unsigned_short (Port);
      end if;
   end Port_To_Network_Port;

   --------------------
   -- Create_Profile --
   --------------------

   procedure Create_Profile
     (Buffer : in out Buffer_Descriptor;
      Profile : out Profile_Ptr)
   is
      use Broca.Marshalling;

      Res : Profile_IIOP_Ptr;
      Nbr_Seq : CORBA.Unsigned_Long;
      Profile_Length : CORBA.Unsigned_Long;
      Old_Endian : CORBA.Boolean;
      New_Endian : CORBA.Boolean;
   begin
      Res := new Profile_IIOP_Type;

      --  Extract length of the sequence.
      Unmarshall (Buffer, Profile_Length);

      --  Profile_Data is an encapsulation. Extract endian.
      Old_Endian := Get_Endianess (Buffer);
      Unmarshall (Buffer, New_Endian);
      Set_Endianess (Buffer, New_Endian);

      --  Extract version
      Unmarshall (Buffer, Res.IIOP_Version.Major);
      Unmarshall (Buffer, Res.IIOP_Version.Minor);

      if Res.IIOP_Version.Major /= 1
        or else Res.IIOP_Version.Minor > 1
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      Unmarshall (Buffer, Res.Host);
      Unmarshall (Buffer, Res.Port);
      pragma Debug (O ("host: " & To_String (Res.Host)));
      pragma Debug (O ("port: " & CORBA.Unsigned_Short'Image (Res.Port)));
      Res.Network_Port := Port_To_Network_Port (Res.Port);
      Broca.Sequences.Unmarshall (Buffer, Res.Object_Key);

      if Res.IIOP_Version.Minor = 1 then
         Unmarshall (Buffer, Nbr_Seq);
         if Nbr_Seq /= 0 then
            --  Components are not yet handled.
            Broca.Exceptions.Raise_Bad_Param;
         end if;
      end if;

      --  Store profile result.
      Profile := Res.all'Access;

      --  Restore little_endian flag of buffer.
      Set_Endianess (Buffer, Old_Endian);
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

   function Create_Strand (Profile : access Profile_IIOP_Type)
                           return Strand_Ptr;

   function Create_Strand (Profile : access Profile_IIOP_Type)
                           return Strand_Ptr is
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
   function Get_Request_Id (Connection : access Strand_Connection_Type)
                            return CORBA.Unsigned_Long;
   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : in out Buffer_Descriptor);

   procedure Receive
     (Connection : access Strand_Connection_Type;
      Buffer     : in out Buffer_Descriptor);

   procedure Release_Connection
     (Connection : access Strand_Connection_Type);

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : in out Buffer_Descriptor)
   is
      use Sockets.Thin;
      use Interfaces.C;
      Length : Buffer_Index_Type := Full_Size (Buffer);
      Bytes  : Buffer_Type (0 .. Length - 1);
      Result : Interfaces.C.int;
   begin
      Read (Buffer, Bytes);
      pragma Debug (O ("Dump outgoing buffer of length" & Length'Img));
      Broca.Buffers.Dump (Bytes);
      Result := C_Send
        (Connection.Strand.Fd,
         Bytes'Address,
         Interfaces.C.int (Length), 0);
      if Result /= Interfaces.C.int (Length) then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;
      pragma Debug (O ("Message correctly sent"));
   end Send;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Connection : access Strand_Connection_Type;
      Buffer     : in out Buffer_Descriptor)
   is
      Length : constant Buffer_Index_Type := Size_Left (Buffer);
   begin
      if Length = 0 then
         pragma Debug (O ("Null length in Receive"));
         return;
      end if;

      declare
         use Sockets.Thin;
         use Interfaces.C;

         Bytes  : Buffer_Type (0 .. Length - 1);
         Result : Interfaces.C.int;
      begin
         Result := C_Recv
           (Connection.Strand.Fd,
            Bytes'Address,
            Interfaces.C.int (Length), 0);

         if Result /=  Interfaces.C.int (Length) then
            Broca.Exceptions.Raise_Comm_Failure;
         end if;
         Write (Buffer, Bytes);

         pragma Debug (O ("Dump incoming buffer of length" & Length'Img));
         Broca.Buffers.Dump (Bytes);
      end;
   end Receive;

   function Get_Request_Id (Connection : access Strand_Connection_Type)
                            return CORBA.Unsigned_Long
   is
      Res : CORBA.Unsigned_Long;
   begin
      Res := Connection.Strand.Request_Id;
      Connection.Strand.Request_Id := Connection.Strand.Request_Id + 1;
      return Res;
   end Get_Request_Id;

   procedure Release_Connection (Connection : access Strand_Connection_Type) is
   begin
      Connection.Strand.Lock.Unlock;
   end Release_Connection;

   --  Find a free connection (or create a new one) for a message to an
   --  OBJECT via PROFILE.
   function Find_Connection (Profile : access Profile_IIOP_Type)
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
         Strand := Create_Strand (Profile);
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
      Encapsulate_Profile'Access,
      Decapsulate_Profile'Access);
end Broca.IIOP;
