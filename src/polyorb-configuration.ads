------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O N F I G U R A T I O N                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

--  PolyORB runtime configuration facility.

--  $Id$

with PolyORB.Dynamic_Dict;
pragma Elaborate_All (PolyORB.Dynamic_Dict);

package PolyORB.Configuration is

   pragma Elaborate_Body;

   Default_Filename  : constant String := "/usr/local/etc/polyorb.conf";
   Filename_Variable : constant String := "POLYORB_CONF";
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
   --  Section and variable names are case-insensitive.

   --  Variables in section [environment] can be overridden by setting
   --  environment variables by the same name. Furthermore, each
   --  time a resolved in that section value starts with 'file:',
   --  the contents of the file is used instead.

   type Configuration_Function is
     access procedure (Key, Value : String);
   --  A function that sets variable Key to value Value within
   --  a given configuration section.

   package Configuration_Sections is
      new PolyORB.Dynamic_Dict (Configuration_Function);
   --  All known configuration sections.

   ---------------------------------------------
   -- Operations related to the [environment] --
   -- configuration section.                  --
   ---------------------------------------------

   Environment_Configuration_Section : constant String
     := "environment";

   function Get_Conf (Key : String; Default : String := "")
     return String;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined.

   Naming_Host         : constant String := "POLYORB_NAMING_HOST";
   Naming_Port         : constant String := "POLYORB_NAMING_PORT";
   Naming_IOR          : constant String := "POLYORB_NAMING_IOR";

   Naming_Host_Default : constant String := "localhost";
   Naming_Port_Default : constant String := "4161";
   Naming_IOR_Default  : constant String := "";
   --  Naming_Host is used to locate the COSNaming service from within
   --  Resolve_Initial_Reference. Naming_Port is used for creating and
   --  locating the COSNaming service from Register_Initial_Reference and
   --  Resolve_Initial_Reference. Naming_Host_Default and Naming_Port_Default
   --  can be used as a default when no explicit information is given.

   Port                : constant String := "POLYORB_PORT";
   Port_Default        : constant String := "0";
   --  Port to use for an internet server. If none is specified, Port_Default
   --  is used unless it is "0", in which case an system-assigned
   --  one will be used.

   Requesting_Principal         : constant String := "POLYORB_PRINCIPAL";
   Requesting_Principal_Default : constant String := "nobody";
   --  Default principle file to use. Useful for GNOME.

end PolyORB.Configuration;
