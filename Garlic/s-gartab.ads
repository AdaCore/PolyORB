------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T A B L E                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Types;

package System.Garlic.Table is

   generic
      type Index_Type is range <>;
      Null_Index     : Index_Type;
      First_Index    : Index_Type;

      Initial_Size   : Positive;
      Increment_Size : Natural;

      type Component_Type is private;
      Null_Component : Component_Type;

   package Complex is

      --  These procedures are atomic and cannot be aborted

      procedure Differ (Version : in Types.Version_Id);
      --  Block until internal Version becomes different from Version.

      procedure Enter;
      --  Lock table.

      function  Get_Component (N : Index_Type) return Component_Type;
      --  Check whether component of index N corresponds to an allocated
      --  component. When N is not allocated, allocate it. Raise
      --  Constraint_Error when N is not in range of current table.

      function  Get_Index (S : String) return Index_Type;
      --  Check whether this name is already related to a component index.
      --  If not, allocate a component, associate its index to its name
      --  and return its index.

      function  Get_Name (N : Index_Type) return String;
      --  Return the name related to component of index N. Return an
      --  empty string when this index corresponds to a non-allocated
      --  component.

      procedure Initialize;

      function Last return Index_Type;
      --  Return last index used.

      procedure Leave (Version : out Types.Version_Id);
      --  Unlock table. Return internal version for later use. Version is
      --  updated by Set_Component.

      procedure Leave;
      --  Unlock table.

      procedure Set_Component (N : Index_Type; C : Component_Type);
      --  Set component of index N to C. When N is not allocated, allocate
      --  it. Raise Constraint_Error when N is not in range of current table.

      procedure Set_Name  (N : Index_Type; S : String);
      --  Set component name of index N to S. When N is not allocated, allocate
      --  it. Raise Constraint_Error when N is not in range of current table.

      procedure Update;
      --  Modify version

   end Complex;

   generic
      type Index_Type is range <>;
      Null_Index     : Index_Type;
      First_Index    : Index_Type;

      Initial_Size   : Positive;
      Increment_Size : Natural;

      type Component_Type is private;
      Null_Component : Component_Type;

   package Medium is

      type Component_Table_Type is
         array (Index_Type range <>) of Component_Type;

      type Component_Table_Access is access Component_Table_Type;

      Table : Component_Table_Access;

      --  These procedures are atomic and cannot be aborted

      function  Get_Component (N : Index_Type) return Component_Type;
      --  Check whether component of index N corresponds to an allocated
      --  component. When N is not allocated, allocate it. Raise
      --  Constraint_Error when N is not in range of current table.

      function  Get_Index (S : String) return Index_Type;
      --  Check whether this name is already related to a component index.
      --  If not, allocate a component, associate its index to its name
      --  and return its index.

      function  Get_Name (N : Index_Type) return String;
      --  Return the name related to component of index N. Return an
      --  empty string when this index corresponds to a non-allocated
      --  component.

      procedure Initialize;

      procedure Set_Component (N : Index_Type; C : Component_Type);
      --  Set component of index N to C. When N is not allocated, allocate
      --  it. Raise Constraint_Error when N is not in range of current table.

      procedure Set_Name  (N : Index_Type; S : String);
      --  Set component name of index N to S. When N is not allocated, allocate
      --  it. Raise Constraint_Error when N is not in range of current table.

   end Medium;

   generic
      type Index_Type     is range <>;
      Null_Index     : Index_Type;
      First_Index    : Index_Type;

      Initial_Size   : Positive;
      Increment_Size : Positive;

      type Component_Type is private;
      Null_Component : Component_Type;

   package Simple is

      type Component_Table_Type is
         array (Index_Type range <>) of Component_Type;

      type Component_Table_Access is access Component_Table_Type;

      Table : Component_Table_Access;

      function  Allocate return Index_Type;

      procedure Initialize;

   end Simple;

end System.Garlic.Table;
