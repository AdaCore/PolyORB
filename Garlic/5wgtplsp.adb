------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--  S Y S T E M . G A R L I C . T C P . P L A T F O R M _ S P E C I F I C   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with System.Garlic.Thin;

package body System.Garlic.TCP.Platform_Specific is

   --  Windows NT version of this package.

   --  Wrappers functions: these wrappers functions provides a more BSD
   --  compliant version of the sockets interface under Windows.

   --  Note: the layout of this package is quite unconventionnal, but given
   --  the specific goal it tries to achieve, it is acceptable.

   use System.Garlic.Thin;
   package C renames Interfaces.C;

   type NT_Hostent is record
      H_Name      : Strings.chars_ptr;
      H_Aliases   : Chars_Ptr_Pointers.Pointer;
      H_Addrtype  : C.short;
      H_Length    : C.short;
      H_Addr_List : In_Addr_Access_Pointers.Pointer;
   end record;
   pragma Convention (C, NT_Hostent);
   --  Host entry

   type NT_Hostent_Access is access all NT_Hostent;
   pragma Convention (C, NT_Hostent_Access);
   --  Access to host entry

   --------------
   -- C_Accept --
   --------------

   function C_Accept
     (S       : C.int;
      Addr    : Sockaddr_Access;
      Addrlen : access C.int)
      return C.int;
   pragma Export (C, C_Accept, "accept");

   function Std_Accept
     (S       : C.int;
      Addr    : Sockaddr_Access;
      Addrlen : access C.int)
      return C.int;
   pragma Import (Stdcall, Std_Accept, "accept");

   function C_Accept
     (S       : C.int;
      Addr    : Sockaddr_Access;
      Addrlen : access C.int)
      return C.int is
   begin
      return Std_Accept (S, Addr, Addrlen);
   end C_Accept;

   ------------
   -- C_Bind --
   ------------

   function C_Bind
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : C.int)
      return C.int;
   pragma Export (C, C_Bind, "bind");

   function Std_Bind
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : C.int)
      return C.int;
   pragma Import (Stdcall, Std_Bind, "bind");

   function C_Bind
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : C.int)
      return C.int is
   begin
      return Std_Bind (S, Name, Namelen);
   end C_Bind;

   -------------
   -- C_Close --
   -------------

   function C_Close (Fildes : C.int) return C.int;
   pragma Export (C, C_Close, "close");

   function Std_Close (Fildes : C.int) return C.int;
   pragma Import (Stdcall, Std_Close, "closesocket");

   function C_Close (Fildes : C.int) return C.int is
   begin
      return Std_Close (Fildes);
   end C_Close;

   ---------------
   -- C_Connect --
   ---------------

   function C_Connect
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : C.int)
      return C.int;
   pragma Export (C, C_Connect, "connect");

   function Std_Connect
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : C.int)
      return C.int;
   pragma Import (Stdcall, Std_Connect, "connect");

   function C_Connect
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : C.int)
      return C.int is
   begin
      return Std_Connect (S, Name, Namelen);
   end C_Connect;

   ---------------------
   -- C_Gethostbyaddr --
   ---------------------

   function C_Gethostbyaddr
     (Addr     : Strings.chars_ptr;
      Length   : C.int;
      Typ      : C.int)
      return Hostent_Access;
   pragma Export (C, C_Gethostbyaddr, "gethostbyaddr");

   function Std_Gethostbyaddr
     (Addr     : Strings.chars_ptr;
      Length   : C.int;
      Typ      : C.int)
      return NT_Hostent_Access;
   pragma Import (Stdcall, Std_Gethostbyaddr, "gethostbyaddr");

   function C_Gethostbyaddr
     (Addr     : Strings.chars_ptr;
      Length   : C.int;
      Typ      : C.int)
      return Hostent_Access is
      NT_H : NT_Hostent_Access;
   begin
      NT_H := Std_Gethostbyaddr (Addr, Length, Typ);
      return new Hostent'(NT_H.H_Name, NT_H.H_Aliases,
                          C.int (NT_H.H_Addrtype), C.int (NT_H.H_Length),
                          NT_H.H_Addr_List);
   end C_Gethostbyaddr;

   ---------------------
   -- C_Gethostbyname --
   ---------------------

   function C_Gethostbyname
     (Name : Strings.chars_ptr)
      return Hostent_Access;
   pragma Export (C, C_Gethostbyname, "gethostbyname");

   function Std_Gethostbyname
     (Name : Strings.chars_ptr)
      return NT_Hostent_Access;
   pragma Import (Stdcall, Std_Gethostbyname, "gethostbyname");

   function C_Gethostbyname
     (Name : Strings.chars_ptr)
      return Hostent_Access is
      NT_H : NT_Hostent_Access;
   begin
      NT_H := Std_Gethostbyname (Name);
      return new Hostent'(NT_H.H_Name, NT_H.H_Aliases,
                          C.int (NT_H.H_Addrtype), C.int (NT_H.H_Length),
                          NT_H.H_Addr_List);
   end C_Gethostbyname;

   -------------------
   -- C_Gethostname --
   -------------------

   function C_Gethostname
     (Name    : Strings.chars_ptr;
      Namelen : C.int)
      return C.int;
   pragma Export (C, C_Gethostname, "gethostname");

   function Std_Gethostname
     (Name    : Strings.chars_ptr;
      Namelen : C.int)
      return C.int;
   pragma Import (Stdcall, Std_Gethostname, "gethostname");

   function C_Gethostname
     (Name    : Strings.chars_ptr;
      Namelen : C.int)
      return C.int is
   begin
      return Std_Gethostname (Name, Namelen);
   end C_Gethostname;

   -------------------
   -- C_Getsockname --
   -------------------

   function C_Getsockname
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : access C.int)
      return C.int;
   pragma Export (C, C_Getsockname, "getsockname");

   function Std_Getsockname
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : access C.int)
      return C.int;
   pragma Import (Stdcall, Std_Getsockname, "getsockname");

   function C_Getsockname
     (S       : C.int;
      Name    : Sockaddr_Access;
      Namelen : access C.int)
      return C.int is
   begin
      return Std_Getsockname (S, Name, Namelen);
   end C_Getsockname;

   ------------------
   -- C_Getsockopt --
   ------------------

   function C_Getsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Strings.chars_ptr;
      Optlen  : access C.int)
      return C.int;
   pragma Export (C, C_Getsockopt, "getsockopt");

   function Std_Getsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Strings.chars_ptr;
      Optlen  : access C.int)
      return C.int;
   pragma Import (Stdcall, Std_Getsockopt, "getsockopt");

   function C_Getsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Strings.chars_ptr;
      Optlen  : access C.int)
      return C.int is
   begin
      return Std_Getsockopt (S, Level, Optname, Optval, Optlen);
   end C_Getsockopt;

   -----------------
   -- C_Inet_Addr --
   -----------------

   function C_Inet_Addr
     (Cp : Strings.chars_ptr)
      return Interfaces.Unsigned_32;
   pragma Export (C, C_Inet_Addr, "inet_addr");

   function Std_Inet_Addr
     (Cp : Strings.chars_ptr)
      return Interfaces.Unsigned_32;
   pragma Import (Stdcall, Std_Inet_Addr, "inet_addr");

   function C_Inet_Addr
     (Cp : Strings.chars_ptr)
      return Interfaces.Unsigned_32 is
   begin
      return Std_Inet_Addr (Cp);
   end C_Inet_Addr;

   --------------
   -- C_Listen --
   --------------

   function C_Listen (S, Backlog : C.int) return C.int;
   pragma Export (C, C_Listen, "listen");

   function Std_Listen (S, Backlog : C.int) return C.int;
   pragma Import (Stdcall, Std_Listen, "listen");

   function C_Listen (S, Backlog : C.int) return C.int is
   begin
      return Std_Listen (S, Backlog);
   end C_Listen;

   ------------
   -- C_Recv --
   ------------

   function C_Recv
     (S     : C.int;
      Buf   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int)
      return C.int;
   pragma Export (C, C_Recv, "recv");

   function Std_Recv
     (S     : C.int;
      Buf   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int)
      return C.int;
   pragma Import (Stdcall, Std_Recv, "recv");

   function C_Recv
     (S     : C.int;
      Buf   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int)
      return C.int is
   begin
      return Std_Recv (S, Buf, Len, Flags);
   end C_Recv;

   ------------
   -- C_Send --
   ------------

   function C_Send
     (S     : C.int;
      Msg   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int)
      return C.int;
   pragma Export (C, C_Send, "send");

   function Std_Send
     (S     : C.int;
      Msg   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int)
     return C.int;
   pragma Import (Stdcall, Std_Send, "send");

   function C_Send
     (S     : C.int;
      Msg   : Strings.chars_ptr;
      Len   : C.int;
      Flags : C.int)
     return C.int is
   begin
      return Std_Send (S, Msg, Len, Flags);
   end C_Send;

   --------------
   -- C_Setsid --
   --------------

   function Setsid return C.int;
   pragma Export (C, Setsid);
   --  Dummy function that does not exist under NT

   function Setsid return C.int is
   begin
      return 1;
   end Setsid;

   ------------------
   -- C_Setsockopt --
   ------------------

   function C_Setsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Address;
      Optlen  : C.int)
      return C.int;
   pragma Export (C, C_Setsockopt, "setsockopt");

   function Std_Setsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Address;
      Optlen  : C.int)
      return C.int;
   pragma Import (Stdcall, Std_Setsockopt, "setsockopt");

   function C_Setsockopt
     (S       : C.int;
      Level   : C.int;
      Optname : C.int;
      Optval  : Address;
      Optlen  : C.int)
     return C.int is
   begin
      return Std_Setsockopt (S, Level, Optname, Optval, Optlen);
   end C_Setsockopt;

   ----------------
   -- C_Shutdown --
   ----------------

   function C_Shutdown
     (S   : C.int;
      How : C.int)
      return C.int;
   pragma Export (C, C_Shutdown, "shutdown");

   function Std_Shutdown
     (S   : C.int;
      How : C.int)
      return C.int;
   pragma Import (Stdcall, Std_Shutdown, "shutdown");

   function C_Shutdown
     (S   : C.int;
      How : C.int)
      return C.int is
   begin
      return Std_Shutdown (S, How);
   end C_Shutdown;

   --------------
   -- C_Socket --
   --------------

   function C_Socket (Domain, Typ, Protocol : C.int) return C.int;
   pragma Export (C, C_Socket, "socket");

   function Std_Socket (Domain, Typ, Protocol : C.int) return C.int;
   pragma Import (Stdcall, Std_Socket, "socket");

   function C_Socket (Domain, Typ, Protocol : C.int) return C.int is
   begin
      return Std_Socket (Domain, Typ, Protocol);
   end C_Socket;

end System.Garlic.TCP.Platform_Specific;
