------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . P A R A M E T E R S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
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

--  PolyORB runtime configuration facility.

pragma Ada_2005;

with PolyORB.Utils;

package PolyORB.Parameters is

   pragma Preelaborate;

   ------------------
   -- Consumer API --
   ------------------

   --  Used by modules that retrieve configuration information

   function Get_Conf
     (Section, Key : String;
      Default : String := "") return String;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined.

   function Get_Conf
     (Section, Key : String;
      Default      : Boolean := False) return Boolean;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as a Boolean:
   --  * True if the value starts with '1' or 'Y' or 'y',
   --    or is "on" or "enable" or "true"
   --  * False if the value starts with '0' or 'n' or 'N',
   --    or is "off" or "disable" or "false" or empty.
   --  Constraint_Error is raised if the value is set to anything else.
   --  (see also PolyORB.Utils.Strings.To_Boolean).

   function Get_Conf
     (Section, Key : String;
      Default      : Duration := 0.0) return Duration;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as an integer
   --  milliseconds duration.
   --  Constraint_Error is raised if the value is set to anything else.

   function Get_Conf
     (Section, Key : String;
      Default      : Integer := 0) return Integer;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as the decimal
   --  representation of an integer number.
   --  Constraint_Error is raised if the value is set to anything else.

   subtype Interval is Utils.Interval;

   function Get_Conf
     (Section, Key : String;
      Default      : Interval := (0, 0)) return Interval;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as an integer interval
   --  defined by its bounds in decimal representation, separated by an hyphen.
   --  If a single integer is given, it is used as both the low and high
   --  bounds.
   --  Constraint_Error is raised if the value is set to anything else.

   function Make_Global_Key (Section, Key : String) return String;
   --  Build dynamic key from (Section, Key) tuple

   ------------------
   -- Provider API --
   ------------------

   --  Used by modules that provide configuration information

   type Parameters_Source is abstract tagged limited private;
   type Parameters_Source_Access is access all Parameters_Source'Class;

   function Get_Conf
     (Source       : access Parameters_Source;
      Section, Key : String) return String is abstract;
   --  Return the value of the global variable Key in the specified Section.
   --  For unknown (Section, Key) couples, an empty string shall be returned.

   procedure Register_Source (Source : Parameters_Source_Access);
   --  Register one source of configuration parameters. Sources are queried
   --  at run time in the order they were registered, and first match wins,
   --  so sources registered earlier take precedence over sources registered
   --  later.

private

   type Parameters_Source is abstract tagged limited null record;

   type Conf_Filter is access function (Val : String) return String;

   Fetch_From_File_Hook : Conf_Filter := null;
   --  The fetch-from-file hook allows the value of a configuration parameter
   --  to be loaded indirectly from a file; this is independent of the use of a
   --  PolyORB configuration file as a source of configuration parameters (but
   --  both facilities are provided by the PolyORB.Parameters.File package).

   Expand_Macros_Hook : Conf_Filter := null;
   --  The expand-macros hook allows macros to be subsituted within the value
   --  of a configuration parameter.

   procedure Initialize;
   --  Complete the initialization of the configuration parameters framework,
   --  after all sources have been initialized.
   --  See PolyORB.Parameters.Initialization.

end PolyORB.Parameters;
