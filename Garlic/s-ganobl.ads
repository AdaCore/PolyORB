--
--  $Id$
--

with Interfaces.C.Strings;
with System.Garlic.Thin;

package System.Garlic.Non_Blocking is

   --  This package provides replacement routine for some System.Garlic.Thin
   --  subprograms which are potentially blocking. This allows some units to
   --  work even on SunOS platforms where the pthreads library doesn't
   --  provide thread-blocking I/Os.

   package C renames Interfaces.C;
   package Strings renames C.Strings;

   function C_Accept
     (S       : C.int;
      Addr    : Thin.Sockaddr_Access;
      Addrlen : access C.int)
     return C.int;
   --  Thread blocking accept.

   function C_Connect
     (S       : C.int;
      Name    : Thin.Sockaddr_Access;
      Namelen : C.int)
     return C.int;
   --  Thread blocking connect.

   function C_Read
     (Filedes : C.int;
      Buf     : Strings.chars_ptr;
      Nbyte   : C.int)
     return C.int;
   --  Thread blocking read.

   function C_Write
     (Fildes : C.int;
      Buf    : Strings.chars_ptr;
      Nbyte  : C.int)
     return C.int;
   --  Thread blocking write.

end System.Garlic.Non_Blocking;


