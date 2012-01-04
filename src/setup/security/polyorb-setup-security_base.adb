------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E T U P . S E C U R I T Y _ B A S E           --
--                                                                          --
--                                 B o d y                                  --
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
