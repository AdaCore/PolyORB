------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . S O C K E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Exceptions;
with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with Interfaces.C;                    use Interfaces.C;
with Interfaces.C.Strings;            use Interfaces.C.Strings;
with System.Garlic.Constants;
with System.Garlic.TCP_Operations;
with System.Garlic.Soft_Links;        use System.Garlic.Soft_Links;
with System.Garlic.Thin;              use System.Garlic.Thin;
with System.Garlic.Utils;             use System.Garlic.Utils;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body System.Garlic.Sockets is

   use type C.int;

   package Net renames System.Garlic.TCP_Operations;

   type Socket_Set_Record is new Fd_Set;

   procedure Free is
      new Ada.Unchecked_Deallocation
        (Socket_Set_Record, Socket_Set_Type);

   Modes : constant array (Mode_Type) of C.int
     := (Sock_Stream => Constants.Sock_Stream,
         Sock_Dgram  => Constants.Sock_Dgram);

   Options : constant array (Option_Type) of C.int
     := (SO_REUSEADDR => Constants.So_Reuseaddr,
         SO_KEEPALIVE => Constants.So_Keepalive);

   function Port_To_Network (Port : C.unsigned_short)
      return C.unsigned_short;
   pragma Inline (Port_To_Network);
   --  Convert a port number into a network port number

   function Network_To_Port (Net_Port : C.unsigned_short)
     return C.unsigned_short
     renames Port_To_Network;
   --  Symetric operation


   subtype Inet_Addr_Comp_Type is Natural range 0 .. 255;
   type Inet_Addr_Type is array (Natural range <>) of Inet_Addr_Comp_Type;

   Hex_To_Char : constant String (1 .. 16) := "0123456789ABCDEF";

   function Image
     (Val : Inet_Addr_Type;
      Hex : Boolean := False)
     return String;

   type Inet_Addr_V4_Type is new Inet_Addr_Type (1 ..  4);

   type Sock_Inet_Addr_V4_Type is new Sock_Addr_Type with
      record
         Host : Inet_Addr_V4_Type;
      end record;

   function Image (Value : Sock_Inet_Addr_V4_Type) return String;
   --  The dotted form corresponding to an IP address

   procedure Bind_Socket
     (Socket : in Socket_Type;
      MyAddr : in Sock_Inet_Addr_V4_Type);
   --  int bind(int sockfd, struct sockaddr * my_addr, socklen_t addrlen);
   --  Raise Socket_Error on error.

   procedure Connect_Socket
     (Socket : in Socket_Type;
      Server : in Sock_Inet_Addr_V4_Type);
   --  int connect (int sockfd, const struct sockaddr * serv_addr,
   --  socklen_t addrlen);
   --  Raise Socket_Error on error.


   Default_Buffer_Size : constant := 16384;

   type Host_Entry (Length : Natural) is
      record
         Name : String (1 .. Length);
         Addr : Inet_Addr_V4_Type;
      end record;
   --  A reduced host structure.

   Null_Host_Entry : constant Host_Entry := (0, "", (others => 0));

   function Name_To_Host_Entry (Name : String)
     return Host_Entry;
   --  Host entry of an IP name

   function Addr_To_Host_Entry (Addr : Inet_Addr_V4_Type)
     return Host_Entry;
   --  Host entry of an IPv4 address

   function To_Host_Entry (Image : String)
     return Host_Entry;
   --  Host entry of an IP name or a dotted form

   function Is_IPv4_Address (Image : String)
     return Boolean;
   --  Return True if the name looks like an IP address, False otherwise

   function Is_IPv6_Address (Image : String)
     return Boolean;
   --  Return False (IPv6 not yet implemented)

   function To_In_Addr (Addr : Inet_Addr_V4_Type) return Thin.In_Addr;
   --  Convert an IP address to a In_Addr structure

   function Value (Image : String) return Inet_Addr_Type;
   --  The IP address corresponding to a dotted form

   procedure Free is
      new Ada.Unchecked_Deallocation (char_array, char_array_access);

   function Parse_Entry (Host : Hostent)
     return Host_Entry;
   --  Parse an entry

   procedure Raise_Naming_Error
     (Errno   : in Integer;
      Message : in String);
   --  Raise the exception Naming_Error with an appropriate error message

   function To_Host_Addr (Addr : Thin.In_Addr) return Inet_Addr_V4_Type;
   --  Convert a In_Addr structure to an IP address

   function To_Host_Addr (Addr : String) return Inet_Addr_V4_Type;
   --  The IP address corresponding to a dotted form


   -----------
   -- Image --
   -----------

   function Image (Socket : Socket_Type) return String is
   begin
      return Socket'Img;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Image : String)
     return Sock_Addr_Type'Class
   is
      A : Sock_Inet_Addr_V4_Type;
   begin
      A.Port := Null_Port;
      if Is_IPv4_Address (Image) then
         A.Host := To_Host_Addr (Image);
      elsif Is_IPv6_Address (Image) then
         null;
      else
         A.Host := Name_To_Host_Entry (Image).Addr;
      end if;
      return A;
   end Value;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String
   is
      Name : aliased char_array (1 .. 64);
      Res  : constant int := C_Gethostname (Name'Address, Name'Length);
   begin
      if Res = Failure then
         Raise_Naming_Error (Errno, "");
      end if;
      return To_Ada (Name);
   end Host_Name;

   -------------------
   -- Official_Name --
   -------------------

   function Official_Name (Image : String)
     return String
   is
      Hostent : Host_Entry := To_Host_Entry (Image);
   begin
      if Hostent.Length = 0 then
         Ada.Exceptions.Raise_Exception
           (Naming_Error'Identity, "No name for " & Image);
      end if;
      To_Lower (Hostent.Name);
      return Hostent.Name;
   end Official_Name;

   -----------
   -- Clear --
   -----------

   procedure Clear (Set : in out Socket_Set_Type; Socket : in Socket_Type) is
   begin
      if Set = null then
         Set := new Socket_Set_Record'(0);
      end if;
      Set.all := Set.all xor 2 ** Natural (Socket);
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set   (Set : in out Socket_Set_Type; Socket : in Socket_Type) is
   begin
      if Set = null then
         Set := new Socket_Set_Record'(0);
      end if;
      Set.all := Set.all or 2 ** Natural (Socket);
   end Set;

   ----------
   -- Zero --
   ----------

   procedure Zero  (Set : in out Socket_Set_Type) is
   begin
      if Set /= null then
         Free (Set);
      end if;
   end Zero;

   -----------
   -- Empty --
   -----------

   function Empty
     (Set : Socket_Set_Type) return Boolean is
   begin
      return (Set = null
        or else Set.all = 0);
   end Empty;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Set    : Socket_Set_Type;
      Socket : Socket_Type) return Boolean is
   begin
      return (Set /= null
        and then (Set.all and 2 ** Natural (Socket)) /= 0);
   end Is_Set;

   ----------------
   -- New_Socket --
   ----------------

   function New_Socket
     (Mode : Mode_Type := SOCK_STREAM)
     return Socket_Type
   is
      Result : C.int;

   begin
      Result := Net.C_Socket (Constants.Af_Inet, Modes (Mode), 0);
      if Result = Failure then
         return Null_Socket;
      end if;
      return Socket_Type (Result);
   end New_Socket;

   -------------------
   -- Accept_Socket --
   -------------------

   function Accept_Socket
     (Socket  : Socket_Type)
     return Socket_Type
   is
      Res : C.int;
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Sin.Sin_Family := Constants.Af_Inet;
      Res := Net.C_Accept (C.int (Socket), Sin'Address, Len'Access);
      if Res = Failure then
         raise Socket_Error;
      end if;
      return Socket_Type (Res);
   end Accept_Socket;

   ------------------
   -- Close_Socket --
   ------------------

   procedure Close_Socket
     (Socket : in Socket_Type)
   is
      Res : C.int;

   begin
      Res := Net.C_Close (C.int (Socket));
   end Close_Socket;

   ---------------------
   -- Get_Socket_Name --
   ---------------------

   function Get_Socket_Name
     (Socket : Socket_Type)
     return Sock_Addr_Type'Class
   is
      Sin  : aliased Sockaddr_In;
      Len  : aliased C.int := Sin'Size / 8;
      Addr : Sock_Inet_Addr_V4_Type;

   begin
      if C_Getsockname (C.int (Socket), Sin'Address, Len'Access) = Failure then
         raise Socket_Error;
      end if;
      Addr.Host (1) := Inet_Addr_Comp_Type (Sin.Sin_Addr.S_B1);
      Addr.Host (2) := Inet_Addr_Comp_Type (Sin.Sin_Addr.S_B2);
      Addr.Host (3) := Inet_Addr_Comp_Type (Sin.Sin_Addr.S_B3);
      Addr.Host (4) := Inet_Addr_Comp_Type (Sin.Sin_Addr.S_B4);
      Addr.Port     := Port_Type (Network_To_Port (Sin.Sin_Port));
      return Addr;
   end Get_Socket_Name;

   -------------------
   -- Listen_Socket --
   -------------------

   procedure Listen_Socket
     (Socket : in Socket_Type;
      Length : in Positive := 15) is
   begin
      if C_Listen (C.int (Socket), C.int (Length)) = Failure then
         raise Socket_Error;
      end if;
   end Listen_Socket;

   --------------------
   -- Receive_Socket --
   --------------------

   procedure Receive_Socket
     (Socket : in Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Addr : System.Address := Item'Address;
      Len  : C.int          := Item'Length;
      Res  : C.int;

   begin
      Res := Net.C_Recv (C.int (Socket), Addr, Len, 0);
      if Res <= 0 then
         Last := Item'First - 1;
      else
         Last := Item'First + Ada.Streams.Stream_Element_Offset (Res);
      end if;
   end Receive_Socket;

   -------------------
   -- Select_Socket --
   -------------------

   procedure Select_Socket
     (R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Timeout      : in Microseconds := Forever)
   is
      Res  : C.int;
      Len  : C.int;
      Set  : Fd_Set;
      RSet : aliased Fd_Set;
      WSet : aliased Fd_Set;
      TVal : aliased Timeval := (0, Timeval_Unit (Timeout));
      TPtr : Timeval_Access;

   begin
      if Timeout = Forever then
         TPtr := null;
      else
         TPtr := TVal'Unchecked_Access;
      end if;

      if R_Socket_Set = null then
         RSet := 0;
      else
         RSet := Fd_Set (R_Socket_Set.all);
      end if;
      Set := RSet;

      if W_Socket_Set = null then
         WSet := 0;
      else
         WSet := Fd_Set (W_Socket_Set.all);
         if WSet > Set then
            Set := WSet;
         end if;
      end if;

      if Set = 0 then
         raise Socket_Error;
      end if;

      Len := 0;
      while Set /= 0 loop
         Len := Len + 1;
         Set := Set / 2;
      end loop;

      Res := C_Select
        (Len, RSet'Unchecked_Access, WSet'Unchecked_Access, null, TPtr);

      if R_Socket_Set /= null then
         R_Socket_Set.all := Socket_Set_Record (RSet);
      end if;
      if W_Socket_Set /= null then
         W_Socket_Set.all := Socket_Set_Record (WSet);
      end if;
   end Select_Socket;

   -----------------
   -- Send_Socket --
   -----------------

   procedure Send_Socket
     (Socket : in  Socket_Type;
      Item   : in  Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Addr : System.Address := Item'Address;
      Len  : C.int          := Item'Length;
      Res  : C.int;

   begin
      Res := Net.C_Send (C.int (Socket), Addr, Len, 0);
      if Res <= 0 then
         Last := Item'First - 1;
      else
         Last := Item'First + Ada.Streams.Stream_Element_Offset (Res);
      end if;
   end Send_Socket;

   -----------------------
   -- Set_Socket_Option --
   -----------------------

   procedure Set_Socket_Option
     (Socket : in Socket_Type;
      Option : in Option_Type)
   is
      One : aliased C.int := 1;
      Len : aliased C.int := One'Size / 8;

   begin
      if C_Setsockopt
        (C.int (Socket),
         Constants.Sol_Socket,
         Options (Option),
         One'Address, Len) = Failure then
         raise Socket_Error;
      end if;
   end Set_Socket_Option;

   ---------------------
   -- Port_To_Network --
   ---------------------

   function Port_To_Network (Port : C.unsigned_short)
     return C.unsigned_short
   is
      use type C.unsigned_short;
   begin
      if Default_Bit_Order = High_Order_First then

         --  No conversion needed. On these platforms, htons() defaults
         --  to a null procedure.

         return Port;
      else

         --  We need to swap the high and low byte on this short to make
         --  the port number network compliant.

         return (Port / 256) + (Port mod 256) * 256;
      end if;
   end Port_To_Network;

   -----------
   -- Image --
   -----------

   function Image
     (Val : Inet_Addr_Type;
      Hex : Boolean := False)
     return String
   is
      --  A Host_Addr_Comp_Type image has at most a length of 4

      Buffer    : String (1 .. 4 * Val'Length);
      Length    : Natural := 1;
      Separator : Character;

      procedure Img10 (V : Inet_Addr_Comp_Type);
      procedure Img10 (V : Inet_Addr_Comp_Type) is
         Img : constant String := V'Img;
         Len : Natural := Img'Length - 1;
      begin
         Buffer (Length .. Length + Len - 1) := Img (2 .. Img'Last);
         Length := Length + Len;
      end Img10;

      procedure Img16 (V : Inet_Addr_Comp_Type);
      procedure Img16 (V : Inet_Addr_Comp_Type) is
      begin
         Buffer (Length)     := Hex_To_Char (Natural (V / 16) + 1);
         Buffer (Length + 1) := Hex_To_Char (Natural (V mod 16) + 1);
         Length := Length + 2;
      end Img16;

   begin
      if Hex then
         Separator := ':';
      else
         Separator := '.';
      end if;
      for I in Val'Range loop
         if Hex then
            Img16 (Val (I));
         else
            Img10 (Val (I));
         end if;
         if I /= Val'Last then
            Buffer (Length) := Separator;
            Length := Length + 1;
         end if;
      end loop;
      return Buffer (1 .. Length - 1);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Sock_Inet_Addr_V4_Type) return String is
   begin
      return Image (Inet_Addr_Type (Value.Host), Hex => False);
   end Image;

   -----------------
   -- Bind_Socket --
   -----------------

   procedure Bind_Socket
     (Socket : in Socket_Type;
      MyAddr : in Sock_Inet_Addr_V4_Type)
   is
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Sin.Sin_Family := Constants.Af_Inet;
      Sin.Sin_Port   := Port_To_Network (C.unsigned_short (MyAddr.Port));
      if C_Bind (C.int (Socket), Sin'Address, Len) = Failure then
         raise Socket_Error;
      end if;
   end Bind_Socket;

   --------------------
   -- Connect_Socket --
   --------------------

   procedure Connect_Socket
     (Socket : in Socket_Type;
      Server : in Sock_Inet_Addr_V4_Type)
   is
      Sin : aliased Sockaddr_In;
      Len : aliased C.int := Sin'Size / 8;

   begin
      Sin.Sin_Family    := Constants.Af_Inet;
      Sin.Sin_Addr.S_B1 := C.unsigned_char (Server.Host (1));
      Sin.Sin_Addr.S_B2 := C.unsigned_char (Server.Host (2));
      Sin.Sin_Addr.S_B3 := C.unsigned_char (Server.Host (3));
      Sin.Sin_Addr.S_B4 := C.unsigned_char (Server.Host (4));
      Sin.Sin_Port      := Port_To_Network (C.unsigned_short (Server.Port));
      if Net.C_Connect (C.int (Socket), Sin'Address, Len) = Failure then
         raise Socket_Error;
      end if;
   end Connect_Socket;

   ------------------------
   -- Name_To_Host_Entry --
   ------------------------

   function Name_To_Host_Entry (Name : String)
     return Host_Entry
   is
      RA : Hostent_Access;
      HN : char_array := To_C (Name);
   begin
      Soft_Links.Enter_Critical_Section;
      RA := C_Gethostbyname (HN);
      if RA = null then
         Soft_Links.Leave_Critical_Section;
         Raise_Naming_Error (Errno, Name);
      end if;
      declare
         HE : constant Host_Entry := Parse_Entry (RA.all);
      begin
         Soft_Links.Leave_Critical_Section;
         return HE;
      end;
   end Name_To_Host_Entry;

   ------------------------
   -- Addr_To_Host_Entry --
   ------------------------

   function Addr_To_Host_Entry (Addr : Inet_Addr_V4_Type)
     return Host_Entry
   is
      Add : aliased In_Addr := To_In_Addr (Addr);
      Res : Hostent_Access;
   begin
      Soft_Links.Enter_Critical_Section;
      Res := C_Gethostbyaddr (Add'Address,
                              C.int (Add'Size / CHAR_BIT),
                              Constants.Af_Inet);
      if Res = null then
         Soft_Links.Leave_Critical_Section;
         Raise_Naming_Error (Errno, Image (Inet_Addr_Type (Addr)));
      end if;
      declare
         Result : constant Host_Entry := Parse_Entry (Res.all);
      begin
         Soft_Links.Leave_Critical_Section;
         return Result;
      end;
   end Addr_To_Host_Entry;

   -------------------
   -- To_Host_Entry --
   -------------------

   function To_Host_Entry (Image : String)
     return Host_Entry
   is
   begin
      if Is_IPv4_Address (Image) then
         declare
            Addr : Inet_Addr_Type := Value (Image);
         begin
            return Addr_To_Host_Entry (Inet_Addr_V4_Type (Addr));
         end;
      elsif Is_IPv6_Address (Image) then
         return Null_Host_Entry;
      else
         return Name_To_Host_Entry (Image);
      end if;
   end To_Host_Entry;

   ---------------------
   -- Is_IPv4_Address --
   ---------------------

   function Is_IPv4_Address (Image : String)
     return Boolean
   is
   begin
      for Index in Image'Range loop
         declare
            Current : Character renames Image (Index);
         begin
            if (Current < '0'
                or else Current > '9')
              and then Current /= '.' then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_IPv4_Address;

   ---------------------
   -- Is_IPv6_Address --
   ---------------------

   function Is_IPv6_Address (Image : String)
     return Boolean
   is
   begin
      return False;
   end Is_IPv6_Address;

   -----------------
   -- Parse_Entry --
   -----------------

   function Parse_Entry (Host : Hostent)
     return Host_Entry
   is
      C_Addr : constant In_Addr_Access_Array
        := In_Addr_Access_Pointers.Value (Host.H_Addr_List);

      Name   : constant String := Value (Host.H_Name);
      Result : Host_Entry (Name'Length);
   begin
      Result.Name := Name;
      Result.Addr := To_Host_Addr (C_Addr (C_Addr'First).all);
      return Result;
   end Parse_Entry;

   ------------------------
   -- Raise_Naming_Error --
   ------------------------

   procedure Raise_Naming_Error
     (Errno   : in Integer;
      Message : in String)
   is

      function Error_Message return String;
      --  Return the message according to Errno.

      -------------------
      -- Error_Message --
      -------------------

      function Error_Message return String is
      begin
         case Errno is
            when Constants.Host_Not_Found => return "Host not found";
            when Constants.Try_Again      => return "Try again";
            when Constants.No_Recovery    => return "No recovery";
            when Constants.No_Address     => return "No address";
            when others                   => return "Unknown error" &
                                                    Integer'Image (Errno);
         end case;
      end Error_Message;

   begin
      Ada.Exceptions.Raise_Exception
        (Naming_Error'Identity, Error_Message & ": " & Message);
   end Raise_Naming_Error;

   ------------------
   -- To_Host_Addr --
   ------------------

   function To_Host_Addr (Addr : In_Addr) return Inet_Addr_V4_Type is
   begin
      return (1 => Inet_Addr_Comp_Type (Addr.S_B1),
              2 => Inet_Addr_Comp_Type (Addr.S_B2),
              3 => Inet_Addr_Comp_Type (Addr.S_B3),
              4 => Inet_Addr_Comp_Type (Addr.S_B4));
   end To_Host_Addr;

   ----------------
   -- To_In_Addr --
   ----------------

   function To_In_Addr (Addr : Inet_Addr_V4_Type) return Thin.In_Addr is
   begin
      return (S_B1 => unsigned_char (Addr (1)),
              S_B2 => unsigned_char (Addr (2)),
              S_B3 => unsigned_char (Addr (3)),
              S_B4 => unsigned_char (Addr (4)));
   end To_In_Addr;

   ------------------
   -- To_Host_Addr --
   ------------------

   function To_Host_Addr (Addr : String) return Inet_Addr_V4_Type
   is
      function Convert is
         new Ada.Unchecked_Conversion
            (Source => Interfaces.Unsigned_32,
             Target => In_Addr);
      Converted : constant In_Addr := Convert (C_Inet_Addr (To_C (Addr)));
   begin
      return (1 => Inet_Addr_Comp_Type (Converted.S_B1),
              2 => Inet_Addr_Comp_Type (Converted.S_B2),
              3 => Inet_Addr_Comp_Type (Converted.S_B3),
              4 => Inet_Addr_Comp_Type (Converted.S_B4));
   end To_Host_Addr;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Inet_Addr_Type
   is
      function Convert is
         new Ada.Unchecked_Conversion (Source => Interfaces.Unsigned_32,
                                       Target => In_Addr);
      Converted : constant In_Addr := Convert (C_Inet_Addr (To_C (Image)));
   begin
      return (1 => Inet_Addr_Comp_Type (Converted.S_B1),
              2 => Inet_Addr_Comp_Type (Converted.S_B2),
              3 => Inet_Addr_Comp_Type (Converted.S_B3),
              4 => Inet_Addr_Comp_Type (Converted.S_B4));
   end Value;

end System.Garlic.Sockets;
