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
with Broca.Sockets;

with Sockets.Constants;
with Sockets.Naming;
with Sockets.Thin;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.IIOP is

   package Constants renames Standard.Sockets.Constants;
   package Naming renames Standard.Sockets.Naming;
   package Thin renames Standard.Sockets.Thin;

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iiop");
   procedure O is new Broca.Debug.Output (Flag);

   --  This connection provides a strand to communicate using a given
   --  transport endpoint.

   type Strand_Connection_Type is new Connection_Type with
      record
         Strand : Strand_Type;
      end record;

   No_Strand : constant Strand_Type
     := (List   => null,
         Socket => Thin.Failure);

   function Receive
     (Connection : access Strand_Connection_Type;
      Length     : Opaque.Index_Type)
     return Opaque.Octet_Array_Ptr;

   procedure Release
     (Connection : access Strand_Connection_Type);

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : access Buffer_Type);

   function Image (Profile : access Profile_IIOP_Type) return String;
   --  For debugging purpose ...

   function Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type)
     return Profile_Ptr;

   function Port_To_Network_Port
     (Port : CORBA.Unsigned_Short)
     return Interfaces.C.unsigned_short;
   --  To translate a CORBA short into a network port.

   subtype Hash_Type is Natural range 0 .. 30;

   function Hash_String is new GNAT.HTable.Hash (Hash_Type);

   function Hash_Strand  (K : Strand_Key) return Hash_Type;
   function Equal_Strand (K1, K2 : Strand_Key) return Boolean;

   package SHT is
      new GNAT.HTable.Simple_HTable
        (Hash_Type,
         Strand_List_Ptr,
         null,
         Strand_Key,
         Hash_Strand,
         Equal_Strand);

   --  We need to store profiles and strands into hash tables in order
   --  to avoid duplicating profiles from refs that we have already
   --  unmarshalled. We can have several strands for a given profile
   --  in order to serve several methods of the same object at the
   --  same time.

   procedure Finalize
     (The_List : in out Strand_List) is
   begin
      pragma Debug (O ("Finalize (Strand_List)"));

      Enter_Critical_Section;
      SHT.Remove (The_List.Key);

      declare
         Socks : Strand_Sequences.Element_Array
           := Strand_Sequences.To_Element_Array
           (Strand_Sequences.Sequence (The_List.L));
      begin
         Leave_Critical_Section;
         for I in Socks'Range loop
            Thin.C_Close (Socks (I).Socket);
         end loop;
      end;
   end Finalize;

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
      use Naming;
      use Constants;
      use Thin;

      use Broca.Refs;

      use Strand_Sequences;

      Key : constant Strand_Key
        := (Host => Profile.Host,
            Port => Profile.Port);

      The_Strand_List : Strand_List_Ptr;

      Strand  : Strand_Type;

   begin
      Enter_Critical_Section;

      --  Create the strand list node if needed and register it into
      --  the strands hash table.

      The_Strand_List := SHT.Get (Key);

      if The_Strand_List = null then
         pragma Debug (O ("init strand list for profile " & Image (Profile)));

         The_Strand_List := new Strand_List;
         The_Strand_List.Key := Key;
         SHT.Set (Key, The_Strand_List);
      end if;

      if Is_Nil (Profile.Strands) then
         pragma Debug (O ("Creating ref to strand list"));
         Set (Profile.Strands, Broca.Refs.Entity_Ptr (The_Strand_List));
      else
         pragma Debug (O ("Checking ref to strand list"));
         pragma Assert (Entity_Of (Profile.Strands)
                        = Broca.Refs.Entity_Ptr (The_Strand_List));
         --  If a profile has a non-nil reference to a strands list,
         --  and SHT contains a list with the same key, ensure
         --  consistency.
         null;
      end if;

      --  Try to reuse an existing strand

      if Length (The_Strand_List.L) > 0 then
         pragma Debug (O ("reuse strand from profile " & Image (Profile)));

         Strand := Element_Of (The_Strand_List.L, 1);
         Delete (The_Strand_List.L, 1, 1);
         Leave_Critical_Section;
      else
         Leave_Critical_Section;

         --  No strands available: create a new one.

         --  FIXME: Add a mechanism to close idle strands.

         pragma Debug (O ("create new strand for profile " & Image (Profile)));

         declare
            Addr : Sockaddr_In;
            Host : String := To_Standard_String (Profile.Host);
         begin
            Strand.List := The_Strand_List;

            Strand.Socket := C_Socket (Af_Inet, Sock_Stream, 0);
            if Strand.Socket = Failure then
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

               pragma Debug (O ("fail to connect socket for profile " &
                                Image (Profile)));

               Broca.Exceptions.Raise_Comm_Failure;
            end if;
         end;
      end if;

      return new Strand_Connection_Type'
        (Connection_Type with Strand => Strand);
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

   ---------------------------
   -- Marshall_Profile_Body --
   ---------------------------

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Profile_IIOP_Type)
   is
      use Broca.CDR;

      Profile_Body : Buffer_Access
        := new Buffer_Type;

   begin
      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.
      Start_Encapsulation (Profile_Body);

      --  Version
      Marshall (Profile_Body, IIOP_Version.Major);
      Marshall (Profile_Body, IIOP_Version.Minor);

      Marshall (Profile_Body, Profile.Host);
      Marshall (Profile_Body, Profile.Port);
      Marshall (Profile_Body, Profile.ObjKey);

      --  Marshall the Profile_Body into Buffer.
      Marshall (Buffer, Encapsulate (Profile_Body));
      Release (Profile_Body);
   end Marshall_Profile_Body;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type)
     return Profile_Ptr
   is
      Version : Version_Type;
      Length  : CORBA.Long;

      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : aliased Buffer_Type;

      Host   : CORBA.String;
      Port   : CORBA.Unsigned_Short;
      ObjKey : Broca.Sequences.Octet_Sequence;
      Result : Profile_IIOP_Access;

   begin
      pragma Debug (O ("Unmarshall_IIOP_Profile_Body : enter"));
      Decapsulate (Profile_Body'Access, Profile_Buffer'Access);

      Version.Major := Unmarshall (Profile_Buffer'Access);
      Version.Minor := Unmarshall (Profile_Buffer'Access);

      if Version.Major /= IIOP_Version.Major
        or else Version.Minor > IIOP_Version.Minor
      then
         pragma Debug (O ("Unmarshall_IIOP_Profile_Body : "
                          & "Invalid IIOP version number"));
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      Host   := Unmarshall (Profile_Buffer'Access);
      Port   := Unmarshall (Profile_Buffer'Access);
      ObjKey := Unmarshall (Profile_Buffer'Access);

      if Version.Minor = 1 then
         Length := Unmarshall (Profile_Buffer'Access);
         if Length /= 0 then
            --  FIXME: Multiple components are not yet handled.
            Broca.Exceptions.Raise_Bad_Param;
         end if;
      end if;

      Result := new Profile_IIOP_Type;
      
      Result.Version := Version;
      Result.Host    := Host;
      Result.Port    := Port;
      Result.ObjKey  := ObjKey;

      pragma Debug (O ("Created profile: " & Image (Result)));

      return Profile_Ptr (Result);
   end Unmarshall_IIOP_Profile_Body;

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
     return Opaque.Octet_Array_Ptr
   is
      use Broca.Opaque;

      Result : Octet_Array_Ptr := new Octet_Array (1 .. Length);

   begin
      if Length = 0 then
         pragma Debug (O ("null length in Receive"));

         return Result;
      end if;

      if
        Sockets.Receive (Connection.Strand.Socket, Result, Length) /= Length
      then
         Free (Result);
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      return Result;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release
     (Connection : access Strand_Connection_Type) is
   begin
      pragma Debug (O ("release strand"));

      --  Push the free strand into the strands list.

      Enter_Critical_Section;
      Append
        (Connection.Strand.List.L,
         Connection.Strand);
      Connection.Strand := No_Strand;
      Leave_Critical_Section;
   end Release;

   ----------
   -- Send --
   ----------

   procedure Send
     (Connection : access Strand_Connection_Type;
      Buffer     : access Buffer_Type)
   is
      use Thin;
      use Interfaces.C;

   begin
      pragma Debug (O ("dump outgoing buffer"));
      Broca.Buffers.Show (Buffer.all);

      begin
         Broca.Buffers.IO_Operations.Write_To_FD
           (Connection.Strand.Socket, Buffer);

      exception when others =>
         Broca.Exceptions.Raise_Comm_Failure;
      end;

      pragma Debug (O ("message correctly sent"));
   end Send;

begin

   Broca.IOP.Register
     (Tag_Internet_IOP,
      Unmarshall_IIOP_Profile_Body'Access);

end Broca.IIOP;
