------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . P O A _ P O L I C I E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  Base types for the various configuration axes (policies)
--  of the PolyORB Portable Object Adapter (libreally inspired from
--  the POA specification in CORBA).

--  $Id$

with Sequences.Unbounded;

with PolyORB.POA_Types;
with PolyORB.Dynamic_Dict;
pragma Elaborate_All (PolyORB.Dynamic_Dict);

package PolyORB.POA_Policies is

   --  No proper body: no elaboration control.

   type Policy is abstract tagged null record;
   type Policy_Access is access all Policy'Class;

   package Policy_Sequences is new Sequences.Unbounded (Policy_Access);
   subtype PolicyList is Policy_Sequences.Sequence;
   type PolicyList_Access is access all PolicyList;

   package Policy_Repository is
      new PolyORB.Dynamic_Dict (PolyORB.POA_Policies.Policy_Access);

   function Policy_Id (Self : Policy) return String is abstract;

   procedure Check_Compatibility
     (Self : Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
      is abstract;
   --  Check the compatibility of the current policy with the
   --  other policies of the object adapter.

   --  function Create return Policy_Access is abstract;
   --  The creation function, implemented for each type of policy.
   --  Useless because no controlling operand and no controlling result.

   procedure Free
     (P   : in     Policy;
      Ptr : in out Policy_Access)
     is abstract;

end PolyORB.POA_Policies;
