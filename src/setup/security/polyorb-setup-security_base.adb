------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E T U P . S E C U R I T Y _ B A S E           --
--                                                                          --
--                                 B o d y                                  --
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

--  Neutral Core Setup

--  with PolyORB.Security.Authorization_Elements;
--  pragma Warnings (Off, PolyORB.Security.Authorization_Elements);

--  with PolyORB.Security.Credentials.GSSUP;
--  pragma Warnings (Off, PolyORB.Security.Credentials.GSSUP);

--  with PolyORB.Security.Credentials.X509;
--  pragma Warnings (Off, PolyORB.Security.Credentials.X509);

--  with PolyORB.Security.Identities.Anonymous;
--  pragma Warnings (Off, PolyORB.Security.Identities.Anonymous);

--  with PolyORB.Security.Identities.Principal_Name;
--  pragma Warnings (Off, PolyORB.Security.Identities.Principal_Name);

--  with PolyORB.Security.Privilege_Authorities;
--  pragma Warnings (Off, PolyORB.Security.Privilege_Authorities);

with PolyORB.Security.Security_Manager;
pragma Warnings (Off, PolyORB.Security.Security_Manager);

--  GIOP Protocol Personality Setup

with PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
pragma Warnings (Off, PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List);

with PolyORB.GIOP_P.Tagged_Components.Null_Tag;
pragma Warnings (Off, PolyORB.GIOP_P.Tagged_Components.Null_Tag);

--  with PolyORB.GIOP_P.Tagged_Components.SECIOP_Sec_Trans;
--  pragma Warnings (Off, PolyORB.GIOP_P.Tagged_Components.SECIOP_Sec_Trans);

with PolyORB.Setup.TLSIOP;
pragma Warnings (Off, PolyORB.Setup.TLSIOP);

package body PolyORB.Setup.Security_Base is

end PolyORB.Setup.Security_Base;
