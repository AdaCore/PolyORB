with Ada.Strings.Unbounded;
with CORBA; use CORBA;
with Broca.Exceptions;
with Broca.Marshalling;
with Broca.Sequences;
with Sockets.Constants;
with Sockets.Naming;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.IIOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iiop");
   procedure O is new Broca.Debug.Output (Flag);

   function Port_To_Network_Port (Port : CORBA.Unsigned_Short)
     return Interfaces.C.unsigned_short;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Profile_Iiop_Type)
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
      Profile : out Broca.Object.Profile_Ptr)
   is
      use Broca.Marshalling;

      Res : Profile_IIOP_Ptr;
      Nbr_Seq : CORBA.Unsigned_Long;
      Profile_Length : CORBA.Unsigned_Long;
      Old_Endian : CORBA.Boolean;
      New_Endian : CORBA.Boolean;
   begin
      Res := new Profile_Iiop_Type;

      --  Extract length of the sequence.
      Unmarshall (Buffer, Profile_Length);

      --  Profile_Data is an encapsulation. Extract endian.
      Old_Endian := Get_Endianess (Buffer);
      Unmarshall (Buffer, New_Endian);
      Set_Endianess (Buffer, New_Endian);

      --  Extract version
      Unmarshall (Buffer, Res.Iiop_Version.Major);
      Unmarshall (Buffer, Res.Iiop_Version.Minor);

      if Res.Iiop_Version.Major /= 1
        or else Res.Iiop_Version.Minor > 1
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      Unmarshall (Buffer, Res.Host);
      Unmarshall (Buffer, Res.Port);
      pragma Debug (O ("host: " & To_String (Res.Host)));
      pragma Debug (O ("port: " & CORBA.Unsigned_Short'Image (Res.Port)));
      Res.Network_Port := Port_To_Network_Port (Res.Port);
      Broca.Sequences.Unmarshall (Buffer, Res.Object_Key);

      if Res.Iiop_Version.Minor = 1 then
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

   procedure Create_Socket_Address (Profile : in out Profile_Iiop_Type);

   procedure Create_Socket_Address (Profile : in out Profile_Iiop_Type)
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

   function Create_Strand (Profile : access Profile_Iiop_Type)
                           return Strand_Ptr;

   function Create_Strand (Profile : access Profile_Iiop_Type)
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
     (Profile : access Profile_Iiop_Type; Strand : Strand_Ptr);

   procedure Open_Strand (Profile : access Profile_Iiop_Type;
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

   type Strand_Connection_Type is new Broca.Object.Connection_Type with
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
      Length : Buffer_Index_Type := Size (Buffer);
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
      use Sockets.Thin;
      use Interfaces.C;
      Length : Buffer_Index_Type := Size_Left (Buffer);
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
   function Find_Connection (Profile : access Profile_Iiop_Type)
                             return Broca.Object.Connection_Ptr
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
      return new Strand_Connection_Type'(Broca.Object.Connection_Type
                                         with Strand => Strand);
   end Find_Connection;

end Broca.IIOP;
