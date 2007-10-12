------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A W S . P A R A M E T E R S . S E T                    --
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

package AWS.Parameters.Set is

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : String);
   --  Add a new Key/Value pair into the parameter set.

   procedure Add (Parameter_List : in out List; Parameters : String);
   --  Set parameters for the current request. This is used for a POST method
   --  because the parameters are found in the message body and are not known
   --  when we parse the request line. The Parameters string has the form
   --  "name1=value1&name2=value2...". The paramaters are added to the list.
   --  The parameters can start with a '?' (standard Web character separator)
   --  which is just ignored.

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : Boolean);
   --  If Mode is True it will use all parameters with case sensitivity.

   procedure Reset (Parameter_List : in out List);
   --  Removes all object from the Set. Set will be reinitialized and will be
   --  ready for new use.

   procedure Free (Parameter_List : in out List);
   --  Release all memory used by the list.

end AWS.Parameters.Set;
