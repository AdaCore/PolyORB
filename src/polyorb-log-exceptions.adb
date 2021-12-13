------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . L O G . E X C E P T I O N S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2021, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with GNAT.Exception_Actions;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Log.Exceptions is

   use Ada.Exceptions;

   package L is new PolyORB.Log.Facility_Log ("polyorb.log.exceptions");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean renames L.Enabled;

   procedure Initialize;

   procedure Log_Exception (Occ : Exception_Occurrence);
   --  Generate trace for Occ

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if C then
         GNAT.Exception_Actions.Register_Global_Action (Log_Exception'Access);
      end if;
   end Initialize;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception (Occ : Exception_Occurrence) is
   begin
      if Exception_Identity (Occ) = Standard'Abort_Signal'Identity then
         O ("<asynchronous abort>");
      end if;
      O (Exception_Information (Occ));
   end Log_Exception;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"log.exceptions",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Log.Exceptions;
