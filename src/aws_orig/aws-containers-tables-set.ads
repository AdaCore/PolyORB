------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            A W S . C O N T A I N E R S . T A B L E S . S E T             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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
