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

with CORBA;             use CORBA;

with GNAT.HTable;

with Broca.Exceptions;
with Broca.CDR;         use Broca.CDR;
with Broca.Buffers;     use Broca.Buffers;
with Broca.Opaque;
with Broca.Soft_Links;  use Broca.Soft_Links;
with Broca.Buffers.IO_Operations;
with Broca.IOP;         use Broca.IOP;
with Broca.Sequences;   use Broca.Sequences;

with Sockets.Constants;
with Sockets.Naming;
with Sockets.Thin;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.IIOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iiop");
   procedure O is new Broca.Debug.Output (Flag);

   type Strand_Connection_Type is new Connection_Type with
      record
         Strand : Strand_Access;
      end record;

   function Receive
     (Connection : access Strand_Connection_Type;
      Length     : Opaque.Index_Type)
     return Opaque.Octet_Array;

   procedure Release
     (Connection : access Strand_Connection_Type);

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : access Buffer_Type);

   function Image (Profile : access Profile_IIOP_Type) return String;

   procedure Marshall_IIOP_Profile_Body
     (IOR     : access Buffer_Type;
      Profile : access Profile_Type'Class);

   function Port_To_Network_Port
     (Port : CORBA.Unsigned_Short)
     return Interfaces.C.unsigned_short;

   procedure Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type;
      Profile  : out Profile_Ptr);


   subtype Hash_Type is Natural range 0 .. 30;

   function Hash_String is new GNAT.HTable.Hash (Hash_Type);

   type Profile_Key is
      record
         Host   : CORBA.String;
         Port   : CORBA.Unsigned_Short;
         ObjKey : Broca.Sequences.Octet_Sequence;
      end record;

   function Hash_Profile  (K : Profile_Key) return Hash_Type;
   function Equal_Profile (K1, K2 : Profile_Key) return Boolean;

   type Strand_Key is
      record
         Host : CORBA.String;
         Port : CORBA.Unsigned_Short;
      end record;

   function Hash_Strand  (K : Strand_Key) return Hash_Type;
   function Equal_Strand (K1, K2 : Strand_Key) return Boolean;

   package PHT is
      new GNAT.HTable.Simple_HTable
        (Hash_Type,
         Profile_IIOP_Access,
         null,
         Profile_Key,
         Hash_Profile,
         Equal_Profile);

   package SHT is
      new GNAT.HTable.Simple_HTable
        (Hash_Type,
         Strand_List_Access,
         null,
         Strand_Key,
         Hash_Strand,
         Equal_Strand);

   --------------------
   -- Create_Profile --
   --------------------

   procedure Create_Profile
     (Buffer  : access Buffer_Type;
      Profile : out Profile_Ptr) is
   begin
      Unmarshall_IIOP_Profile_Body (Buffer, Profile);
   end Create_Profile;

   -------------------
   -- Equal_Profile --
   -------------------

   function Equal_Profile
     (K1, K2 : Profile_Key)
     return Boolean
   is
      use Broca.Sequences.Octet_Sequences;

   begin
      return K1.Port = K2.Port
        and then K1.Host = K2.Host
        and then K1.ObjKey = K2.ObjKey;
   end Equal_Profile;

   ------------------
   -- Equal_Strand --
   ------------------

   function Equal_Strand
     (K1, K2 : Strand_Key)
     return Boolean is
   begin
      return K1.Port = K2.Port
        and then K1.Host = K2.Host;
   end Equal_Strand;

   ---------------------
   -- Find_Connection --
   ---------------------

   function Find_Connection
     (Profile : access Profile_IIOP_Type)
     return Connection_Ptr
   is
      use Interfaces.C;
      use Sockets.Naming;
      use Sockets.Constants;
      use Sockets.Thin;

      Strand  : Strand_Access;

   begin
      Enter_Critical_Section;

      if Profile.Strands = null then
         pragma Debug (O ("init strand list for profile " & Image (Profile)));

         declare
            Key : Strand_Key;

         begin
            Key.Host := Profile.Host;
            Key.Port := Profile.Port;

            Profile.Strands := new Strand_List_Type;

            SHT.Set (Key, Profile.Strands);
         end;
      end if;

      if Profile.Strands.Head /= null then
         pragma Debug (O ("reuse strand from profile " & Image (Profile)));

         Strand               := Profile.Strands.Head;
         Profile.Strands.Head := Strand.Next;
         if Profile.Strands.Head = null then
            Profile.Strands.Tail := null;
         end if;
      end if;

      Leave_Critical_Section;

      if Strand = null then
         pragma Debug (O ("create new strand for profile " & Image (Profile)));

         declare
            Addr : Sockaddr_In;
            Host : String := To_Standard_String (Profile.Host);

         begin
            Strand      := new Strand_Type;
            Strand.List := Profile.Strands;

            Strand.Socket := C_Socket (Af_Inet, Sock_Stream, 0);
            if Strand.Socket = Failure then
               Leave_Critical_Section;

               pragma Debug (O ("fail to open socket for profile " &
                                Image (Profile)));

               Broca.Exceptions.Raise_Comm_Failure;
            end if;

            Addr.Sin_Family := Af_Inet;
            Addr.Sin_Port   := Port_To_Network_Port (Profile.Port);
            Addr.Sin_Addr   := To_In_Addr (Address_Of (Host));

            if C_Connect
              (Strand.Socket,
               Addr'Address,
               Addr'Size / 8) = Failure
            then
               C_Close (Strand.Socket);
               Leave_Critical_Section;

               pragma Debug (O ("fail to connect socket  for profile " &
                                Image (Profile)));

               Broca.Exceptions.Raise_Comm_Failure;
            end if;
         end;
      end if;

      return
        new Strand_Connection_Type'(Connection_Type with Strand => Strand);
   end Find_Connection;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Profile_IIOP_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      return Profile.ObjKey;
   end Get_Object_Key;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : Profile_IIOP_Type)
     return Profile_Tag is
   begin
      return Tag_Internet_IOP;
   end Get_Profile_Tag;

   ------------------
   -- Hash_Profile --
   ------------------

   function Hash_Profile
     (K : Profile_Key)
     return Hash_Type is
   begin
      return Hash_String (To_Standard_String (K.Host) & ' ' & K.Port'Img);
   end Hash_Profile;

   -----------------
   -- Hash_Strand --
   -----------------

   function Hash_Strand
     (K : Strand_Key)
     return Hash_Type is
   begin
      return Hash_String (To_Standard_String (K.Host) & ' ' & K.Port'Img);
   end Hash_Strand;

   ------------
   --  Image --
   ------------

   function Image (Profile : access Profile_IIOP_Type) return String is
   begin
      return To_Standard_String (Profile.Host) & Profile.Port'Img & " <...>";
   end Image;

   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Marshall_IIOP_Profile_Body
     (IOR     : access Buffer_Type;
      Profile : access Profile_Type'Class)
   is
      use Broca.CDR;

      IIOP_Profile : Profile_IIOP_Type renames Profile_IIOP_Type (Profile.all);
      Profile_Body : aliased Buffer_Type;

   begin

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.

      Start_Encapsulation (Profile_Body'Access);

      --  Version
      --  FIXME: Version should not be hard-coded.
      Marshall (Profile_Body'Access, CORBA.Octet'(1));
      Marshall (Profile_Body'Access, CORBA.Octet'(0));

      Marshall (Profile_Body'Access, IIOP_Profile.Host);
      Marshall (Profile_Body'Access, IIOP_Profile.Port);
      Marshall (Profile_Body'Access, IIOP_Profile.ObjKey);

      --  Marshall the Profile_Body into IOR.
      Marshall (IOR, Encapsulate (Profile_Body'Access));
      Release (Profile_Body);
   end Marshall_IIOP_Profile_Body;

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
         pragma Debug (O ("null length in Receive"));

         return Result;
      end if;

      declare
         use Sockets.Thin;
         use Interfaces.C;

         Received : Interfaces.C.int;

      begin
         Received := C_Recv
           (Connection.Strand.Socket,
            Result (1)'Address,
            Interfaces.C.int (Length), 0);

         if Received /=  Interfaces.C.int (Length) then
            Broca.Exceptions.Raise_Comm_Failure;
         end if;

         return Result;
      end;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release
     (Connection : access Strand_Connection_Type)
   is
      List : Strand_List_Access;

   begin
      pragma Debug (O ("release strand"));

      Enter_Critical_Section;
      List := Connection.Strand.List;
      if List.Tail /= null then
         List.Tail.Next := Connection.Strand;

      else
         List.Head := Connection.Strand;
      end if;
      List.Tail := Connection.Strand;
      Leave_Critical_Section;
   end Release;

   ----------
   -- Send --
   ----------

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : access Buffer_Type)
   is
      use Sockets.Thin;
      use Interfaces.C;

   begin
      pragma Debug (O ("Dump outgoing buffer"));
      Broca.Buffers.Show (Buffer.all);

      pragma Debug (O ("Use strand FD =" & Connection.Strand.Socket'Img));

      begin
         Broca.Buffers.IO_Operations.Write_To_FD
           (Connection.Strand.Socket, Buffer);

      exception when others =>
         Broca.Exceptions.Raise_Comm_Failure;
      end;

      pragma Debug (O ("Message correctly sent"));
   end Send;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   procedure Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type;
      Profile  : out Profile_Ptr)
   is
      Version : Version_Type;
      Key     : Profile_Key;
      Length  : CORBA.Long;

      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : aliased Buffer_Type;

      Result : Profile_IIOP_Access;

   begin
      Decapsulate (Profile_Body'Access, Profile_Buffer'Access);

      Version.Major := Unmarshall (Profile_Buffer'Access);
      Version.Minor := Unmarshall (Profile_Buffer'Access);

      if Version.Major /= 1
        or else Version.Minor > 1
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      Key.Host   := Unmarshall (Profile_Buffer'Access);
      Key.Port   := Unmarshall (Profile_Buffer'Access);
      Key.ObjKey := Unmarshall (Profile_Buffer'Access);

      if Version.Minor = 1 then
         Length := Unmarshall (Profile_Buffer'Access);
         if Length /= 0 then
            --  FIXME: Multiple components are not yet handled.
            Broca.Exceptions.Raise_Bad_Param;
         end if;
      end if;

      Enter_Critical_Section;

      Result := PHT.Get (Key);
      if Result = null then
         Result := new Profile_IIOP_Type;
         Result.Version := Version;
         Result.Host    := Key.Host;
         Result.Port    := Key.Port;
         Result.ObjKey  := Key.ObjKey;
         PHT.Set (Key, Result);
      end if;

      Leave_Critical_Section;

      Profile := Profile_Ptr (Result);
   end Unmarshall_IIOP_Profile_Body;

begin
   Register
     (Tag_Internet_IOP,
      Marshall_IIOP_Profile_Body'Access,
      Unmarshall_IIOP_Profile_Body'Access);
end Broca.IIOP;
