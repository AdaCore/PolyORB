------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . E N V I R O N M E N T                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package Broca.Environment is

   pragma Elaborate_Body;

   --  The environment mechanism works as follow:
   --     (1) A configuration file is searched for lines looking like
   --             KEY=VALUE
   --         The file name is taken from the environment (ADABROKER_CONF).
   --         If this variable does not exist, /etc/adabroker.conf will be
   --         used.
   --     (2) For every lookup, the environment is searched first then the
   --         configuration file is used. This way, a user can override the
   --         values present in the file with environment variables.

   Default_Filename  : constant String := "/etc/adabroker.conf";
   Filename_Variable : constant String := "ADABROKER_CONF";

   function Get_Conf (Key : String; Default : String := "") return String;
   --  Return the value of the environment variable Key or Default if this
   --  variable is not defined.

   Naming_Host         : constant String := "ADABROKER_NAMING_HOST";
   Naming_Port         : constant String := "ADABROKER_NAMING_PORT";
   Naming_Host_Default : constant String := "localhost";
   Naming_Port_Default : constant String := "4161";
   --  Naming_Host is used to locate the COSNaming service from within
   --  Resolve_Initial_Reference. Naming_Port is used for creating and
   --  locating the COSNaming service from Register_Initial_Reference and
   --  Resolve_Initial_Reference. Naming_Host_Default and Naming_Port_Default
   --  can be used as a default when no explicit information is given.

   Port                : constant String := "ADABROKER_PORT";
   Port_Default        : constant String := "0";
   --  Port to use for an internet server. If none is specified, Port_Default
   --  is used unless it is "0", in which case an system-assigned
   --  one will be used.

end Broca.Environment;
