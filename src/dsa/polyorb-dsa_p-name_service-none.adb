------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . D S A _ P . N A M E _ S E R V I C E . N O N E       --
--                                                                          --
--                                 B o d y                                  --
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

pragma Ada_2012;

with System.RPC;

with PolyORB.Log;
with PolyORB.References;

package body PolyORB.DSA_P.Name_Service.None is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.dsa_p.name_service.none");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   -----------------------
   -- Nameserver_Lookup --
   -----------------------

   overriding function Nameserver_Lookup
     (Name_Ctx : access None_Name_Server;
      Name     : String;
      Kind     : String;
      Initial  : Boolean := True) return PolyORB.References.Ref
   is
      pragma Unreferenced (Name_Ctx);
      pragma Unreferenced (Initial);
   begin
      pragma Debug
        (C, O ("Nameserver_Lookup (" & Name & "." & Kind & "): enter"));

      raise System.RPC.Communication_Error with
            "lookup of " & Kind & " " & Name & " failed (no name server)";
      return PolyORB.References.Nil_Ref;
   end Nameserver_Lookup;

end PolyORB.DSA_P.Name_Service.None;
