------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.SECURITY.AUTHENTICATION_MECHANISMS.GSSUP_CLIENT          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

package PolyORB.Security.Authentication_Mechanisms.GSSUP_Client is

   type GSSUP_Client_Authentication_Mechanism is
     new Client_Authentication_Mechanism with private;

private

   type GSSUP_Client_Authentication_Mechanism is
     new Client_Authentication_Mechanism with null record;

   --  Derived from Client_Authentication_Mechanism

   overriding function Is_Supports
     (Mechanism   : access GSSUP_Client_Authentication_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Boolean;

   overriding function Init_Security_Context
     (Mechanism   : access GSSUP_Client_Authentication_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Ada.Streams.Stream_Element_Array;

end PolyORB.Security.Authentication_Mechanisms.GSSUP_Client;
