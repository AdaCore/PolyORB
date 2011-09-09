------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . L O G . E X C E P T I O N S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2011, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with GNAT.Exception_Actions;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Log.Exceptions is

   use Ada.Exceptions;
   use PolyORB.Log;

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
