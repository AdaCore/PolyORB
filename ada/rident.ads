------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               R I D E N T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the set of restriction identifiers. It is in a
--  separate package from Restrict so that it can be easily used by the
--  binder without dragging in a lot of stuff.

package Rident is

   --  The following enumeration type defines the set of restriction
   --  identifiers not taking a parameter that are implemented in GNAT.
   --  To add a new restriction identifier, add an entry with the name
   --  to be used in the pragma, and add appropriate calls to the
   --  Check_Restriction routine.

   type Restriction_Id is (

      --  The following cases are checked for consistency in the binder

      No_Abort_Statements,                     -- (RM D.7(5), H.4(3))
      No_Access_Subprograms,                   -- (RM H.4(17))
      No_Allocators,                           -- (RM H.4(7))
      No_Asynchronous_Control,                 -- (RM D.9(10))
      No_Delay,                                -- (RM H.4(21))
      No_Dispatch,                             -- (RM H.4(19))
      No_Dynamic_Priorities,                   -- (RM D.9(9))
      No_Exceptions,                           -- (RM H.4(12))
      No_Fixed_Point,                          -- (RM H.4(15))
      No_Floating_Point,                       -- (RM H.4(14))
      No_IO,                                   -- (RM H.4(20))
      No_Implicit_Heap_Allocations,            -- (RM D.8(8), H.4(3))
      No_Local_Allocators,                     -- (RM H.4(8))
      No_Nested_Finalization,                  -- (RM D.7(4))
      No_Protected_Types,                      -- (RM H.4(5))
      No_Recursion,                            -- (RM H.4(22))
      No_Reentrancy,                           -- (RM H.4(23))
      No_Task_Allocators,                      -- (RM D.7(7))
      No_Task_Hierarchy,                       -- (RM D.7(3), H.4(3))
      No_Terminate_Alternatives,               -- (RM D.7(6))
      No_Unchecked_Access,                     -- (RM H.4(18))
      No_Unchecked_Conversion,                 -- (RM H.4(16))
      No_Unchecked_Deallocation,               -- (RM H.4(9))

      --  The following cases do not require partition-wide checks

      Immediate_Reclamation,                   -- (RM H.4(10))
      No_Implementation_Attributes,            -- GNAT
      No_Implementation_Pragmas,               -- GNAT
      No_Elaboration_Code,                     -- GNAT

      Not_A_Restriction_Id);

   --  The following range of Restriction identifiers is checked for
   --  consistency across a partition. The generated ali file is marked
   --  for each entry to show one of three possibilities:
   --
   --    Corresponding restriction is set (so unit does not violate it)
   --    Corresponding restriction is not violated
   --    Corresponding restriction is violated

   subtype Partition_Restrictions is
     Restriction_Id range No_Abort_Statements .. No_Unchecked_Deallocation;

   --  The following enumeration type defines the set of restriction
   --  parameter identifiers taking a parameter that are implemented in
   --  GNAT. To add a new restriction parameter identifier, add an entry
   --  with the name to be used in the pragma, and add appropriate
   --  calls to Check_Restriction.

   --  Note: the GNAT implementation currently only accomodates restriction
   --  parameter identifiers whose expression value is a non-negative
   --  integer. This is true for all language defined parameters.

   type Restriction_Parameter_Id is (
     Max_Asynchronous_Select_Nesting,         -- (RM D.9(18), H.4(3))
     Max_Protected_Entries,                   -- (RM D.9(14))
     Max_Select_Alternatives,                 -- (RM D.9(12))
     Max_Storage_At_Blocking,                 -- (RM D.9(17))
     Max_Task_Entries,                        -- (RM D.9(13), H.4(3))
     Max_Tasks,                               -- (RM D.9(19), H.4(3))
     Not_A_Restriction_Parameter_Id);

end Rident;
