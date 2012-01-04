------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E T U P . S E C U R E _ S E R V E R           --
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

--  Setup Secure Server

with PolyORB.Setup.Secure_Client;
pragma Warnings (Off, PolyORB.Setup.Secure_Client);

--  Neutral Core Setup

with PolyORB.Security.Authentication_Mechanisms.GSSUP_Target;
pragma Warnings (Off, PolyORB.Security.Authentication_Mechanisms.GSSUP_Target);

--  GIOP Protocol Personality Setup

with PolyORB.Setup.Access_Points.TLSIOP;
pragma Warnings (Off, PolyORB.Setup.Access_Points.TLSIOP);

--  CORBA Application Personality Setup (server side only)

with PolyORB.CORBA_P.TSS_State_Machine;
pragma Warnings (Off, PolyORB.CORBA_P.TSS_State_Machine);

--  ATLAS Privilege Authority

--  with PolyORB.Security.Authority_Mechanisms.ATLAS_Target;
--  pragma Warnings (Off, PolyORB.Security.Authority_Mechanisms.ATLAS_Target);

package body PolyORB.Setup.Secure_Server is

end PolyORB.Setup.Secure_Server;
