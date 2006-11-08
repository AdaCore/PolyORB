------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . C O N S T A N T S               --
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

--  This is the version for Solaris

package System.Garlic.Constants is
   Tcp_Nodelay          : constant := 16#0001#;
   Af_Inet              : constant := 16#0002#;
   Pf_Inet              : constant := 16#0002#;
   Sock_Stream          : constant := 16#0002#;
   Sock_Dgram           : constant := 16#0001#;
   Eintr                : constant := 16#0004#;
   Eagain               : constant := 16#000B#;
   Ewouldblock          : constant := 16#000B#;
   Einprogress          : constant := 16#0096#;
   Ealready             : constant := 16#0095#;
   Eisconn              : constant := 16#0085#;
   Econnrefused         : constant := 16#0092#;
   Fndelay              : constant := 16#0004#;
   Fasync               : constant := 16#1000#;
   F_Getfl              : constant := 16#0003#;
   F_Setfl              : constant := 16#0004#;
   F_Setown             : constant := 16#0018#;
   So_Rcvbuf            : constant := 16#1002#;
   So_Reuseaddr         : constant := 16#0004#;
   Sol_Socket           : constant := 16#FFFF#;
   Sigterm              : constant := 16#000F#;
   Sigkill              : constant := 16#0009#;
   O_Rdonly             : constant := 16#0000#;
   O_Wronly             : constant := 16#0001#;
   O_Rdwr               : constant := 16#0002#;
   Host_Not_Found       : constant := 16#0001#;
   Try_Again            : constant := 16#0002#;
   No_Recovery          : constant := 16#0003#;
   No_Data              : constant := 16#0004#;
   No_Address           : constant := 16#0004#;
   Pollin               : constant := 16#0001#;
   Pollpri              : constant := 16#0002#;
   Pollout              : constant := 16#0004#;
   Pollerr              : constant := 16#0008#;
   Pollhup              : constant := 16#0010#;
   Pollnval             : constant := 16#0020#;
   I_Setsig             : constant := 16#5309#;
   S_Rdnorm             : constant := 16#0040#;
   S_Wrnorm             : constant := 16#0004#;
end System.Garlic.Constants;
