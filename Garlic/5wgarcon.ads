------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . C O N S T A N T S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

--  This package is for a MINGW32 NT environment.

package System.Garlic.Constants is

   Af_Inet        : constant := 2;
   Pf_Inet        : constant := 2;
   Sock_Dgram     : constant := 2;
   Sock_Stream    : constant := 1;
   So_Reuseaddr   : constant := 4;
   Sol_Socket     : constant := 16#ffff#;

   Ewouldblock    : constant := 10035;
   Einprogress    : constant := 10036;
   Ealready       : constant := 10037;
   Eisconn        : constant := 10056;
   Econnrefused   : constant := 10061;
   Host_Not_Found : constant := 11001;
   Try_Again      : constant := 11002;
   No_Recovery    : constant := 11003;
   No_Data        : constant := 11004;
   No_Address     : constant := 11004;

   Eintr          : constant := 4;
   Eagain         : constant := 11;
   So_Rcvbuf      : constant := 16#1002#;
   Sigterm        : constant := 15;

   Fioasync       : constant := 16#8004667d#;
   Fasync         : constant := Fioasync;

   O_Rdonly       : constant := 0;
   O_Wronly       : constant := 1;
   O_Rdwr         : constant := 2;

   Tcp_Nodelay    : constant := -1;
   Fndelay        : constant := -1;
   F_Getfl        : constant := -1;
   F_Setfl        : constant := -1;
   F_Setown       : constant := -1;
   Fiossaioown    : constant := F_Setown;
   Sigkill        : constant := -1;
   Pollin         : constant := -1;
   Pollpri        : constant := -1;
   Pollout        : constant := -1;
   Pollerr        : constant := -1;
   Pollhup        : constant := -1;
   Pollnval       : constant := -1;
   I_Setsig       : constant := -1;
   S_Rdnorm       : constant := -1;
   S_Wrnorm       : constant := -1;

end System.Garlic.Constants;
