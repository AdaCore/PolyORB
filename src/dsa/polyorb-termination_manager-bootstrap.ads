------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TERMINATION_MANAGER.BOOTSTRAP                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2011, Free Software Foundation, Inc.          --
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

--  This package starts and setup the termination manager.
--  It also provides functions for getting references to local and remote
--  termination managers.

with PolyORB.Binding_Objects;
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Servants;

package PolyORB.Termination_Manager.Bootstrap is

   type Term_Manager_Ptr is access Term_Manager;

   -------------------
   --  TM Constants --
   -------------------

   --  Because the termination manager is a singleton in its own partition,
   --  it is legitimate to keep here some references to the TM.

   TM_Name_Space  : constant String := "Termination_Manager";
   --  The name used by the well known service started by the termination
   --  manager.

   RACW_Type_Name : constant String :=
                      "polyorb.termination_manager.term_manager";
   --  The type name of the termination manager RACW

   The_TM : Term_Manager_Ptr;
   --  A pointer to the local termination manager

   The_TM_Ref : PolyORB.References.Ref;
   --  A reference to the local termination manager

   The_TM_Oid : PolyORB.Objects.Object_Id_Access;
   --  A pointer to the local termination manager Object_Id

   type Node_Kind is (DSA_Node, DSA_Node_Without_TM, Non_DSA_Node, Unknown);
   --  The kind of nodes we can link to :
   --
   --  * DSA_Node : a DSA partition with a running termination manager.
   --  * DSA_Node_Without_TM : a DSA partition without a termination manager.
   --  * Non_DSA_Node : a node which is not a DSA partition.
   --  * Unknown : we cannot determine the kind of this node.

   --------------------------------------
   -- TM References Handling Utilities --
   --------------------------------------

   procedure Extract_TM_Reference_From_BO
     (BO  :     Binding_Objects.Binding_Object_Access;
      Ref : out References.Ref;
      NK  : out Node_Kind);
   --  Ref is a reference to the termination manager of the partition which BO
   --  links to. NK gives an indication of the kind of the node to which BO
   --  links.

   function Ref_To_Term_Manager_Access (R : References.Ref)
     return Term_Manager_Access;
   --  Convert a Reference to a Term Manager Access

   function Term_Manager_Access_To_Ref (TM : Term_Manager_Access)
     return References.Ref;
   --  Convert a Term Manager Access to a Reference

   procedure Initialize_Termination_Manager;
   --  Initializes the termination algorithm

private

   procedure Initiate_Well_Known_Service
     (S    : Servants.Servant_Access; Name : String);
   --  Initiate a POA reachable by an absolute path of the form: /Name/ and
   --  which returns always the default servant S.

end PolyORB.Termination_Manager.Bootstrap;
