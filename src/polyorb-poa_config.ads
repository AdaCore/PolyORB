------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . P O A _ C O N F I G                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Global POA configuration.

--  $Id$

with PolyORB.POA_Policies;

package PolyORB.POA_Config is

   pragma Elaborate_Body;

   type Configuration_Type is abstract tagged null record;
   type Configuration_Access is access all Configuration_Type'Class;

   procedure Initialize
     (C : Configuration_Type)
      is abstract;
   --  Create all policies available in this configuration,
   --  and register them with policy repository F.

   function Default_Policies
     (C : Configuration_Type)
     return PolyORB.POA_Policies.PolicyList_Access
      is abstract;
   --  Return the list of default OA policies for this configuration.

   procedure Set_Configuration
     (C : Configuration_Access);
   --  Set the configuration for the whole runtime.
   --  May be called only once. C must be non-null.

   function Configuration return Configuration_Access;
   --  The value set by Set_Configuration.

private

   pragma Inline (Set_Configuration);
   pragma Inline (Configuration);

end PolyORB.POA_Config;
