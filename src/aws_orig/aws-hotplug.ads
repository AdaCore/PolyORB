------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          A W S . H O T P L U G                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;
with GNAT.Regexp;

with AWS.Response;
with AWS.Status;

package AWS.Hotplug is

   type Filter_Set is private;

   procedure Register (Filters : in out Filter_Set;
                       Regexp  : String;
                       URL     : String);
   --  Add a Filter in the Filter_Set, the URL will be called if the URI match
   --  the regexp. If Regexp already exist it just replace the current entry.

   procedure Unregister (Filters : in out Filter_Set;
                         Regexp  : String);
   --  Removes a Filter from the Filter_Set. The filter name is defined by the
   --  regular expression. Does nothing if regexp is not found.

   procedure Apply (Filters : Filter_Set;
                    Status  : AWS.Status.Data;
                    Found   :    out Boolean;
                    Data    :    out Response.Data);
   --  Run through the filters and apply the first one for which the regular
   --  expression match the URI. Set Found to True if one filter has been
   --  called and in that case Data contain the answer, otherwise Found is set
   --  to False.

   procedure Move_Up (Filters : Filter_Set;
                      N       : Positive);
   --  Move filters number N up one position, it gives filter number N a
   --  better priority.

   procedure Move_Down (Filters : Filter_Set;
                        N       : Positive);
   --  Move filters number N down one position, it gives filter number N less
   --  priority.

private

   use Ada.Strings.Unbounded;

   type Filter_Data is record
      Regexp_Str : Unbounded_String;
      Regexp     : GNAT.Regexp.Regexp;
      URL        : Unbounded_String;
   end record;

   type Filter_Array is array (Positive range <>) of Filter_Data;
   type Filter_Array_Access is access Filter_Array;

   type Filter_Set is record
      Count : Natural := 0;
      Set   : Filter_Array_Access;
   end record;

end AWS.Hotplug;
