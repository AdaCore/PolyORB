------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.SECURITY.AUTHENTICATION_MECHANISMS.GSSUP_TARGET          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

   procedure Accept_Security_Context
     (Mechanism    : access GSSUP_Target_Authentication_Mechanism;
      Token        :        PolyORB.Security.Types.Stream_Element_Array_Access;
      Success      : out    Boolean;
      Return_Token : out
        PolyORB.Security.Types.Stream_Element_Array_Access;
      Identity     : out    PolyORB.Security.Identities.Identity_Access);

end PolyORB.Security.Authentication_Mechanisms.GSSUP_Target;
