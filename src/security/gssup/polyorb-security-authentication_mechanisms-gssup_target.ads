------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.SECURITY.AUTHENTICATION_MECHANISMS.GSSUP_TARGET          --
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

pragma Ada_2012;

with PolyORB.Types;

package PolyORB.Security.Authentication_Mechanisms.GSSUP_Target is

   type GSSUP_Target_Authentication_Mechanism is
     new Target_Authentication_Mechanism with private;

private

   type GSSUP_Target_Authentication_Mechanism is
     new Target_Authentication_Mechanism with
   record
      Passwd_File : PolyORB.Types.String;
   end record;

   --  Derived from Target_Authentication_Mechanism

   overriding procedure Accept_Security_Context
     (Mechanism    : access GSSUP_Target_Authentication_Mechanism;
      Token        :        PolyORB.Security.Types.Stream_Element_Array_Access;
      Success      : out    Boolean;
      Return_Token : out
        PolyORB.Security.Types.Stream_Element_Array_Access;
      Identity     : out    PolyORB.Security.Identities.Identity_Access);

end PolyORB.Security.Authentication_Mechanisms.GSSUP_Target;
