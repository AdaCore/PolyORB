------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Logging support for PolyORB

--  $Id$

package PolyORB.Log is

   pragma Preelaborate;

   --  Log_Levels are used to classify the importance of messages

   type Log_Level is
     (Debug,
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

   function Get_Log_Level (Facility : in String) return Log_Level;
   --  Returns the user-requested log level for facility Flag.

   generic
      Facility :  String;
   package Facility_Log is

      procedure Output
        (Message : in String;
         Level   : Log_Level := Debug);
      --  Log Message when Level is at least equal to the user-requested
      --  level for Facility.

   end Facility_Log;

   type Configuration_Hook is access
     function (Section, Key, Default : String)
               return String;

   Get_Conf_Hook : Configuration_Hook := null;

end PolyORB.Log;
