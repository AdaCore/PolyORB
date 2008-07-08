------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            A W S . C O N T A I N E R S . T A B L E S . S E T             --
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

package AWS.Containers.Tables.Set is

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : String);
   --  Add a new Key/Value pair into the parameter set.

   procedure Update
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Positive := 1);
   --  Update the N-th Value with the given Name into the Table.
   --  The container could already have more than one value associated with
   --  this name. If there is M values with this Name, then if:
   --     N <= M      => update the value
   --     N  = M + 1  => the pair name=value is appended to the table
   --     N  > M + 1  => Constraint_Error raised

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode  : Boolean);
   --  If Mode is True it will use all parameters with case sensitivity.

   procedure Reset (Table : in out Table_Type);
   --  Removes all object from the Set. Set will be reinitialized and will be
   --  ready for new use.

   procedure Free (Table : in out Table_Type);
   --  Release all memory used by the table.

end AWS.Containers.Tables.Set;
