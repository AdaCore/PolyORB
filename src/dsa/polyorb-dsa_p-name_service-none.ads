------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . D S A _ P . N A M E _ S E R V I C E . N O N E       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2013, Free Software Foundation, Inc.             --
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

package PolyORB.DSA_P.Name_Service.None is

   type None_Name_Server is new Name_Server with null record;

   overriding procedure Initialize
     (Name_Ctx : access None_Name_Server;
      Location : String) is null;

   overriding procedure Nameserver_Register
     (Name_Ctx : access None_Name_Server;
      Name     : String;
      Kind     : String;
      Obj      : PolyORB.References.Ref) is null;

   overriding function Nameserver_Lookup
     (Name_Ctx : access None_Name_Server;
      Name     : String;
      Kind     : String;
      Initial  : Boolean := True) return PolyORB.References.Ref;
   --  No name server: raises COMMUNICATION_ERROR

end PolyORB.DSA_P.Name_Service.None;
