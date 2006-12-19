------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Logging support for PolyORB

package PolyORB.Log is

   pragma Preelaborate;

   --  Log_Levels are used to classify the importance of messages

   type Log_Level is
     (Unknown,
      --  The log level for this facility has not been defined yet.

      Debug,
      --  Developer interest only, should never be displayed
      --  in a production environment.

      Info,
      --  Informational message indicating progress of normal
      --  operation.

      Notice,
      --  Notesworthy message in normal operation.

      Warning,
      --  Indication that a condition may be abnormal
      --  and requires attention.

      Error,
      --  Indication that an abnormal condition has been identified.

      Critical
      --  Indication that an abnormal condition has been
      --  identified, and that immediate attention is required
      --  to resume normal operation.
      );

   --  Generic package providing logging support for one facility.
   --  Note: the user is responsible for ensuring that the lifetime of any
   --  instance of the generic is no less than that of library package
   --  PolyORB.Log.

   generic
      Facility :  String;
   package Facility_Log is

      --  NOTE: these procedures are not thread safe.

      procedure Output
        (Message : String;
         Level   : Log_Level := Debug);
      --  Log Message when Level is at least equal to the user-requested level
      --  for Facility.

      function Enabled (Level : Log_Level := Debug) return Boolean;
      pragma Inline (Enabled);
      --  True when Level is at least equal to the user-requested level
      --  for Facility.

   end Facility_Log;

   ------------------------------------------------------
   -- Integration with runtime configuration subsystem --
   ------------------------------------------------------

   Log_Section       : constant String    := "log";
   Default_Log_Level : constant Log_Level := Notice;

   package Internals is

      procedure Put_Line (S : String);
      --  Output S to stderr.

      --  Note: this function is to be utilised if and only if we cannot
      --  instanciate PolyORB.Log.Facility_Log.

      type Log_Hook_T is access procedure (S : String);

      Log_Hook : Log_Hook_T;

   end Internals;

private

   procedure Flush;
   --  During early initialization (before the logging and configuration
   --  modules are properly initialized), messages are stored in a buffer.
   --  This procedure is called when logging is initialized to process
   --  buffered messages.

end PolyORB.Log;
