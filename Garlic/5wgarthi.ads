------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                   S Y S T E M . G A R L I C . T H I N                    --
--                                                                          --
--                                 S p e c                                  --
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

with Interfaces.C.Pointers;
pragma Warnings (Off, Interfaces.C.Pointers);
with Interfaces.C.Strings;
with System.Garlic.Constants;

package System.Garlic.Thin is

   --  This is the Windows version of s-garthi.ads.

   package C renames Interfaces.C;
   package Strings renames C.Strings;
   generic package Pointers renames C.Pointers;

   use type C.int;
   --  This is an ugly hack to be able to declare the Failure constant
   --  below.

   Success : constant C.int :=  0;
   Failure : constant C.int := -1;

   type Socket_Fd is new C.unsigned;
   pragma Convention (C, Socket_Fd);

   type Fd_Set is private;

   type Fd_Set_Access is access all Fd_Set;
   pragma Convention (C, Fd_Set_Access);

   type Timeval_Unit is new C.long;
   pragma Convention (C, Timeval_Unit);

   type Timeval is record
      Tv_Sec  : Timeval_Unit;
      Tv_Usec : Timeval_Unit;
   end record;
   pragma Convention (C, Timeval);

   type Timeval_Access is access all Timeval;
   pragma Convention (C, Timeval_Access);

   Immediat : constant Timeval := (0, 0);

   type Int_Access is access all C.int;
   pragma Convention (C, Int_Access);
   --  Access to C integers

   type pid_t is new C.int;
   pragma Convention (C, pid_t);

   type mode_t is new C.int;
   pragma Convention (C, mode_t);

   type key_t is new C.int;
   pragma Convention (C, key_t);

   type Chars_Ptr_Array is array (C.size_t range <>) of
     aliased Strings.chars_ptr;

   package Chars_Ptr_Pointers is
      new Pointers (C.size_t, Strings.chars_ptr, Chars_Ptr_Array,
                    Strings.Null_Ptr);
   --  Arrays of C (char *)

   type In_Addr is record
      S_B1, S_B2, S_B3, S_B4 : C.unsigned_char;
   end record;
   pragma Convention (C, In_Addr);
   --  Internet address

   type In_Addr_Access is access all In_Addr;
   pragma Convention (C, In_Addr_Access);
   --  Access to internet address

   Inaddr_Any : aliased constant In_Addr := (others => 0);
   --  Any internet address (all the interfaces)

   type In_Addr_Access_Array is array (Positive range <>)
     of aliased In_Addr_Access;
   pragma Convention (C, In_Addr_Access_Array);
   package In_Addr_Access_Pointers is
     new Pointers (Positive, In_Addr_Access, In_Addr_Access_Array,
                   null);
   --  Array of internet addresses

   type Sockaddr is record
      Sa_Family : C.unsigned_short;
      Sa_Data   : C.char_array (1 .. 14);
   end record;
   pragma Convention (C, Sockaddr);
   --  Socket address

   type Sockaddr_Access is access all Sockaddr;
   pragma Convention (C, Sockaddr_Access);
   --  Access to socket address

   type Sockaddr_In is record
      Sin_Family : C.unsigned_short      := Constants.Af_Inet;
      Sin_Port   : C.unsigned_short      := 0;
      Sin_Addr   : In_Addr               := Inaddr_Any;
      Sin_Zero   : C.char_array (1 .. 8) := (others => C.char'Val (0));
   end record;
   pragma Convention (C, Sockaddr_In);
   --  Internet socket address

   type Sockaddr_In_Access is access all Sockaddr_In;
   pragma Convention (C, Sockaddr_In_Access);
   --  Access to internet socket address

   type Hostent is record
      H_Name      : Strings.chars_ptr;
      H_Aliases   : Chars_Ptr_Pointers.Pointer;
      H_Addrtype  : C.short;
      H_Length    : C.short;
      H_Addr_List : In_Addr_Access_Pointers.Pointer;
   end record;
   pragma Convention (C, Hostent);
   --  Host entry

   type Hostent_Access is access all Hostent;
   pragma Convention (C, Hostent_Access);
   --  Access to host entry

   type Caddr_T is new Strings.chars_ptr;
   --  Type Caddr_T is in fact a (char *)

   type Iovec is record
      Iov_Base : Caddr_T;
      Iov_Len  : C.int;
   end record;
   pragma Convention (C, Iovec);
   --  Iovec C type

   type Iovec_Access is access all Iovec;
   pragma Convention (C, Iovec_Access);
   --  Access to Iovec structure

   type Msghdr is record
      Msg_Name         : Caddr_T;
      Msg_Namelen      : C.int;
      Msg_Iov          : Iovec_Access;
      Msg_Iovlen       : C.int;
      Msg_Accrights    : Caddr_T;
      Msg_Accrightslen : C.int;
   end record;
   pragma Convention (C, Msghdr);
   --  Message header

   type Msghdr_Access is access all Msghdr;
   pragma Convention (C, Msghdr_Access);
   --  Access to message header.

   function C_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : access C.int)
      return C.int;

   function C_Bind
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int)
      return C.int;

   function C_Close (Fildes : C.int) return C.int;

   function C_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int)
      return C.int;

   procedure C_Dup2 (Fildes, Fildes2 : in C.int);

   function C_Gethostbyaddr
     (Addr     : System.Address;
      Length   : C.int;
      Typ      : C.int)
      return Hostent_Access;

   function C_Gethostbyname
     (Name : C.char_array)
      return Hostent_Access;

   function C_Gethostname
     (Name    : System.Address;
      Namelen : C.int)
      return C.int;

   function C_Getsockname
     (S       : C.int;
      Name    : System.Address;
      Namelen : access C.int)
      return C.int;

   function C_Getsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Strings.chars_ptr;
      Optlen  : access C.int)
      return C.int;

   function C_Inet_Addr
     (Cp : C.char_array)
      return Interfaces.Unsigned_32;

   function C_Listen (S, Backlog : C.int) return C.int;

   function C_Open
     (Path  : C.char_array;
      Oflag : C.int;
      Mode  : mode_t := 0)
     return C.int;

   function C_Recv
     (S     : C.int;
      Buf   : System.Address;
      Len   : C.int;
      Flags : C.int)
     return C.int;

   function C_Select
     (Nfds      : C.int;
      Readfds   : Fd_Set_Access;
      Writefds  : Fd_Set_Access;
      Exceptfds : Fd_Set_Access;
      Timeout   : Timeval_Access)
     return C.int;

   procedure Clear  (FS : in out Fd_Set);

   procedure Set    (FS : in out Fd_Set; Socket : in Socket_Fd);

   function  Is_Set (FS : in     Fd_Set; Socket : in Socket_Fd)
     return Boolean;

   function C_Send
     (S     : C.int;
      Buf   : System.Address;
      Len   : C.int;
      Flags : C.int)
     return C.int;

   procedure C_Setsid;

   function C_Setsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Address;
      Optlen  : C.int)
     return C.int;

   function C_Shutdown
     (S   : C.int;
      How : C.int)
      return C.int;

   function C_Socket (Domain, Typ, Protocol : C.int) return C.int;

   function C_System (Command : System.Address) return C.int;

   procedure Initialize;
   procedure Shutdown;

private

   type Socket_Fd_Array is array (C.unsigned range 1 .. 64) of Socket_Fd;
   pragma Convention (C, Socket_Fd_Array);

   type Fd_Set is record
      fd_count : C.unsigned;
      fd_array : Socket_Fd_Array;
   end record;
   pragma Convention (C, Fd_Set);

   pragma Import (Stdcall, C_Accept, "accept");
   pragma Import (Stdcall, C_Bind, "bind");
   pragma Import (C, C_Close, "close");
   pragma Import (Stdcall, C_Connect, "connect");
   pragma Import (C, C_Dup2, "dup2");
   pragma Import (Stdcall, C_Gethostbyaddr, "gethostbyaddr");
   pragma Import (Stdcall, C_Gethostbyname, "gethostbyname");
   pragma Import (Stdcall, C_Gethostname, "gethostname");
   pragma Import (Stdcall, C_Getsockname, "getsockname");
   pragma Import (Stdcall, C_Getsockopt, "getsockopt");
   pragma Import (Stdcall, C_Inet_Addr, "inet_addr");
   pragma Import (Stdcall, C_Listen, "listen");
   pragma Import (C, C_Open, "open");
   pragma Import (Stdcall, C_Recv, "recv");
   pragma Import (Stdcall, C_Select, "select");
   pragma Import (Stdcall, C_Send, "send");
   pragma Import (Stdcall, C_Setsockopt, "setsockopt");
   pragma Import (Stdcall, C_Shutdown, "shutdown");
   pragma Import (Stdcall, C_Socket, "socket");
   pragma Import (C, C_System, "system");

end System.Garlic.Thin;
