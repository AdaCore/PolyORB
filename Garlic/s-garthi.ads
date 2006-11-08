------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                   S Y S T E M . G A R L I C . T H I N                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

   --  This package is intended for hosts implementing BSD sockets with a
   --  standard interface. Although it is named using GNAT conventions for
   --  Linux, it will be used as a default for all the platforms that do not
   --  have a specific version of this file.
   --
   --  If you change this spec you must change all the target specific
   --  implementations too. Here is a list of them:
   --
   --     NT implementation      5wgarthi.ads and .adb

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

   type Timeval_Unit is new C.int;
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
      H_Addrtype  : C.int;
      H_Length    : C.int;
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

   type Two_Int is array (0 .. 1) of C.int;
   pragma Convention (C, Two_Int);
   --  Used with pipe()

   type Pollfd is record
      Fd      : C.int;
      Events  : C.short;
      Revents : C.short;
   end record;
   pragma Convention (C, Pollfd);

   type Pollfd_Array is array (Positive range <>) of Pollfd;
   pragma Convention (C, Pollfd_Array);

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

   procedure C_Dup2 (Fildes, Fildes2 : C.int);

   function C_Fcntl
     (Fildes : C.int;
      Cmd    : C.int;
      Arg    : C.int := 0)
      return C.int;

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

   function C_Getpeername
     (S       : C.int;
      Name    : System.Address;
      Namelen : access C.int)
     return C.int;

   function C_Getpid return pid_t;

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

   function C_Inet_Network
     (Cp : Strings.chars_ptr)
      return Interfaces.Unsigned_32;

   function C_Inet_Makeaddr
     (Net : C.int;
      Lna : C.int)
      return In_Addr;

   function C_Inet_Lnaof (I : In_Addr) return C.int;

   function C_Inet_Netof (I : In_Addr) return C.int;

   function C_Inet_Ntoa (I : In_Addr) return Strings.chars_ptr;

   function C_Ioctl (D : C.int; Request : C.int; Arg : C.int) return C.int;

   function C_Kill (Pid : pid_t; Sig : C.int) return C.int;

   function C_Listen (S, Backlog : C.int) return C.int;

   function C_Msgget
     (Key    : key_t;
      Msgflg : C.int)
      return C.int;

   function C_Msgrcv
     (Msqid  : C.int;
      Msgp   : Strings.chars_ptr;
      Msgsz  : C.int;
      Msgtyp : C.long;
      Msgflg : C.int)
     return C.int;

   function C_Msgsnd
     (Msqid  : C.int;
      Msgp   : Strings.chars_ptr;
      Msgsz  : C.int;
      Msgflg : C.int)
     return C.int;

   function C_Open
     (Path  : C.char_array;
      Oflag : C.int;
      Mode  : mode_t := 0)
     return C.int;

   function C_Pipe (Filedes : access Two_Int) return C.int;

   function C_Poll
     (Fds     : System.Address;
      Nfds    : C.unsigned_long;
      Timeout : C.int)
     return C.int;

   function C_Read
     (Fildes : C.int;
      Buf    : Strings.chars_ptr;
      Nbyte  : C.int)
     return C.int;

   function C_Readv
     (Fildes : C.int;
      Iov    : Iovec_Access;
      Iovcnt : C.int)
     return C.int;

   function C_Recv
     (S     : C.int;
      Buf   : System.Address;
      Len   : C.int;
      Flags : C.int)
     return C.int;

   function C_Recvfrom
     (S       : C.int;
      Buf     : Strings.chars_ptr;
      Len     : C.int;
      Flags   : C.int;
      From    : Sockaddr_Access;
      Fromlen : access C.int)
     return C.int;

   function C_Recvmsg
     (S     : C.int;
      Msg   : Msghdr_Access;
      Flags : C.int)
     return C.int;

   function C_Select
     (Nfds      : C.int;
      Readfds   : Fd_Set_Access;
      Writefds  : Fd_Set_Access;
      Exceptfds : Fd_Set_Access;
      Timeout   : Timeval_Access)
     return C.int;

   function C_Socketpair (Domain   : C.int;
                          Typ      : C.int;
                          Protocol : C.int;
                          SV       : access Two_Int)
     return C.int;

   procedure Clear  (FS : in out Fd_Set);
   --  make FS empty.

   procedure Set    (FS : in out Fd_Set; Socket : Socket_Fd);
   --  add Socket into FS.

   function  Is_Set (FS : Fd_Set; Socket : Socket_Fd)
     return Boolean;
   --  returns True if Socket is set into FS.

   function C_Send
     (S     : C.int;
      Buf   : System.Address;
      Len   : C.int;
      Flags : C.int)
     return C.int;

   function C_Sendmsg
     (S     : C.int;
      Msg   : Msghdr_Access;
      Flags : C.int)
     return C.int;

   function C_Sendto
     (S     : C.int;
      Msg   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int;
      To    : Sockaddr_Access;
      Tolen : C.int)
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

   function C_Strerror (Errnum : C.int) return Strings.chars_ptr;

   function C_System (Command : System.Address) return C.int;

   function C_Write
     (Fildes : C.int;
      Buf    : Strings.chars_ptr;
      Nbyte  : C.int)
     return C.int;

   function C_Writev
     (Fildes : C.int;
      Iov    : Iovec_Access;
      Iovcnt : C.int)
     return C.int;

   procedure Initialize;
   procedure Shutdown;

private

   type Fd_Set is mod 2**32;
   pragma Convention (C, Fd_Set);

   pragma Import (C, C_Accept, "accept");
   pragma Import (C, C_Bind, "bind");
   pragma Import (C, C_Close, "close");
   pragma Import (C, C_Connect, "connect");
   pragma Import (C, C_Dup2, "dup2");
   pragma Import (C, C_Fcntl, "fcntl");
   pragma Import (C, C_Gethostbyaddr, "gethostbyaddr");
   pragma Import (C, C_Gethostbyname, "gethostbyname");
   pragma Import (C, C_Gethostname, "gethostname");
   pragma Import (C, C_Getpeername, "getpeername");
   pragma Import (C, C_Getpid, "getpid");
   pragma Import (C, C_Getsockname, "getsockname");
   pragma Import (C, C_Getsockopt, "getsockopt");
   pragma Import (C, C_Inet_Addr, "inet_addr");
   pragma Import (C, C_Inet_Network, "inet_network");
   pragma Import (C, C_Inet_Makeaddr, "inet_makeaddr");
   pragma Import (C, C_Inet_Lnaof, "inet_lnaof");
   pragma Import (C, C_Inet_Netof, "inet_netof");
   pragma Import (C, C_Inet_Ntoa, "inet_ntoa");
   pragma Import (C, C_Ioctl, "ioctl");
   pragma Import (C, C_Kill, "kill");
   pragma Import (C, C_Listen, "listen");
   pragma Import (C, C_Msgget, "msgget");
   pragma Import (C, C_Msgrcv, "msgrcv");
   pragma Import (C, C_Msgsnd, "msgsnd");
   pragma Import (C, C_Open, "open");
   pragma Import (C, C_Pipe, "pipe");
   pragma Import (C, C_Poll, "poll");
   pragma Import (C, C_Read, "read");
   pragma Import (C, C_Readv, "readv");
   pragma Import (C, C_Recv, "recv");
   pragma Import (C, C_Recvfrom, "recvfrom");
   pragma Import (C, C_Recvmsg, "recvmsg");
   pragma Import (C, C_Select, "select");
   pragma Import (C, C_Send, "send");
   pragma Import (C, C_Sendmsg, "sendmsg");
   pragma Import (C, C_Sendto, "sendto");
   pragma Import (C, C_Setsid, "setsid");
   pragma Import (C, C_Setsockopt, "setsockopt");
   pragma Import (C, C_Socketpair, "socketpair");
   pragma Import (C, C_Shutdown, "shutdown");
   pragma Import (C, C_Socket, "socket");
   pragma Import (C, C_Strerror, "strerror");
   pragma Import (C, C_System, "system");
   pragma Import (C, C_Write, "write");
   pragma Import (C, C_Writev, "writev");

end System.Garlic.Thin;
