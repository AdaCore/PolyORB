------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . D I G E S T                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
