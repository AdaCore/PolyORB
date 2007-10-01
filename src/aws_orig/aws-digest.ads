------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . D I G E S T                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Some utilities for http digest authentication.

with MD5;
pragma Elaborate_All (MD5);

package AWS.Digest is

   subtype Digest_String is MD5.Digest_String;

   function Create_Nonce return String;
   --  Create a Nonce value for the digest authentication
   --  see [RFC-2617 - 3.2.1]

   function Check_Nonce (Value : String) return Boolean;
   --  Check Nonce for validity and expiration.

   function Create_Digest
     (Username, Realm, Password : String;
      Nonce                     : String;
      Method, URI               : String)
      return Digest_String;
   --  Returns a simple MD5 Digest.

   function Create_Digest
     (Username, Realm, Password : String;
      Nonce, NC, CNonce, QOP    : String;
      Method, URI               : String)
      return Digest_String;
   --  Returns a more complex MD5 Digest.

end AWS.Digest;
