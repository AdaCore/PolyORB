------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O N F I G U R A T I O N                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  PolyORB runtime configuration facility.

--  $Id$

package PolyORB.Configuration is

   pragma Elaborate_Body;

   PolyORB_Conf_Default_Filename  : constant String := "polyorb.conf";
   PolyORB_Conf_Filename_Variable : constant String := "POLYORB_CONF";
   Syntax_Error      : exception;

   --  PolyORB supports a global runtime configuration file.
   --  By default, the location of this file is Default_Filename.
   --  This default value can be overridden by setting the environment
   --  named by Filename_Variable.
   --
   --  The syntax of the configuration file is:
   --  - empty lines and lines that have a '#' in column 1 are
   --    ignored;
   --  - sections can be started by lines of the form
   --    '[' SECTION-NAME ']';
   --  - variable assignments can be performed by lines of the
   --    form VARIABLE-NAME '=' VALUE.
   --
   --  Anything else raises Syntax_Error.
   --
   --  Any variable assignment is local to a section.
   --  Assignments that occur before the first section declaration
   --  are relative to section [environment].
   --  Section and variable names are case sensitive.

   --  A variable Var.Iable in section [Sec] can be overridden by
   --  setting environment variable "POLYORB_SEC_VAR_IABLE"
   --  (see Make_Env_Name in body).
   --  Furthermore, each time a resolved in that section value
   --  starts with "file:", the contents of the file is used instead.

   ---------------------------------------------
   -- Operations related to the [environment] --
   -- configuration section.                  --
   ---------------------------------------------

   Environment_Configuration_Section : constant String
     := "environment";

   procedure Load_Configuration_File (Conf_File_Name : String);
   --  Load Conf_File_Name configuration file.

   function Configuration_File_Name return String;
   --  Return PolyORB Configuration file name.

   procedure Initialize;
   --  Initialize Configuration subsystem.

   procedure Set_Conf
     (Section, Key : String;
      Value        : String);
   --  Sets the value of the given Key in the named Section.

   function Get_Conf (Section, Key : String; Default : String := "")
     return String;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined.

   function Get_Conf (Section, Key : String; Default : Boolean := False)
     return Boolean;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as a Boolean:
   --  * True if the value starts with '1' or 'Y' or 'y',
   --    or is "on" or "enable" or "true"
   --  * False if the value starts with '0' or 'n' or 'N',
   --    or is "off" or "disable" or "false" or empty.
   --  Constraint_Error is raised if the value is set to anything else.

   function Get_Conf (Section, Key : String; Default : Integer := 0)
     return Integer;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as the decimal
   --  representation of an integer number.
   --  Constraint_Error is raised if the value is set to anything else.

end PolyORB.Configuration;
