------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        B R O C A . S O C K E T S                         --
--                                                                          --
--                                 S p e c                                  --
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

with Broca.Opaque;
with Interfaces.C;
with System;

package Broca.Sockets is

   pragma Elaborate_Body;

   function Receive
     (Socket : Interfaces.C.int;
      Buffer : access Broca.Opaque.Octet_Array;
      Length : Broca.Opaque.Index_Type)
     return Broca.Opaque.Index_Type;
   --  Call C recv() until everything has been read or an error is returned.
   --  Returns the number of bytes read before the error.

   function Receive
     (Socket : Interfaces.C.int;
      Buffer : System.Address;
      Length : Broca.Opaque.Index_Type)
    return Broca.Opaque.Index_Type;
   --  Similar function but with a System.Address instead

end Broca.Sockets;
