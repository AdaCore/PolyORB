------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . S T O R A G E S . D S M          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2008, Free Software Foundation, Inc.               --
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

--  This package implements a distributed shared memory storage
--  support for shared passive packages. The algorithm used for this
--  implementation is based on the most optimised dynamic distributed
--  manager algorithm with dynamic distributed copy set (i.e K .Li and
--  P. Hudak, Memory Coherence in Shared Virtual Memory Systems, ACM
--  Transactions on Computer Systems, nov 1989, vol. 7, num. 4,
--  p. 321-359).
--  DSM_Manager_Type is a distributed object which manages memory
--  coherence of a shared variable on each node renferencing it. This
--  unit defines DSM_Manager_Type which contains informations about
--  a shared variable state, like access rigths, probable owner, read
--  only copies node refenreces, requiered to running the algorithm.

pragma Ada_2005;

with PolyORB.Utils.Dynamic_Tables;

package PolyORB.DSA_P.Storages.DSM is

   pragma Remote_Types;

   ----------------------
   -- DSM_Manager_Type --
   ----------------------

   --  Manage coherence of a shared passive variable. RACW provides
   --  remote primitives for execution of the Li & Hudak algorithm.

   type DSM_Manager_Type is abstract new Shared_Data_Manager_Type with private;
   type DSM_Manager_RACW is access all DSM_Manager_Type'Class;
   pragma Asynchronous (DSM_Manager_RACW);

   --  Copy_Set_Type is used to track of other partitions managers to which a
   --  readonly copy was sent.

   package Copy_Set_Tables is new PolyORB.Utils.Dynamic_Tables
     (Table_Component_Type => DSM_Manager_RACW,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10);
   subtype Copy_Set_Type is Copy_Set_Tables.Instance;

   --  DSM_Manager_Type type primitives

   --  Remotely called primitives

   procedure Invalidate_Request
     (Self      : access DSM_Manager_Type;
      Rqst_Node : DSM_Manager_RACW;
      Version   : Integer) is abstract;
   --  Invalidation request trigered by a node which obtained write
   --  access on shared variable V. This routines forward the
   --  invalidation request to nodes in its copy set before reset it.

   procedure Write_Request
     (Self        : access DSM_Manager_Type;
      Rqst_Node   : DSM_Manager_RACW) is abstract;
   --  Remote request from node which needs write access on shared
   --  variable V. This is an asynchronous procedure that involves
   --  a Write_Reply remote call. If server node is the owner, it
   --  replyies to request node, otherwise it forwards the request
   --  to its propable owner.

   procedure Write_Reply
     (Self        : access DSM_Manager_Type;
      Var_Data    : SDT.Any_Container_Ptr;
      Read_Copies : Copy_Set_Type;
      Version     : Integer) is abstract;
   --  Remote asynchronous procedure that allows to reply to a write
   --  request node by sending local copy set and shared variable data.

   procedure Read_Request
     (Self       : access DSM_Manager_Type;
      Rqst_Node  : DSM_Manager_RACW) is abstract;
   --  Remote request from node which needs read access on shared
   --  variable V. This is an asynchronous procedure that involves
   --  a Read_Reply remote call. If server node is the owner, it
   --  adds the request node its copy set and replyies to request
   --  node, otherwise it forwards the request to its propable owner.

   procedure Read_Reply
     (Self         : access DSM_Manager_Type;
      Var_Data     : SDT.Any_Container_Ptr;
      Reply_Node   : DSM_Manager_RACW;
      Version      : Integer) is abstract;
   --  Remote asynchronous procedure that allows to reply to a read
   --  request node by sending last writed value of the shared variable.

   function Get_Initial_Owner
     (Self      : access DSM_Manager_Type;
      Var_Name  : String)
      return DSM_Manager_RACW
      is abstract;
   --  Return the intial owner of the varibale Var_Name. It should be
   --  called on a factory of a package.

   procedure Register_Passive_Package
     (Pkg_Name : String;
      Is_Owner : Boolean;
      Location : String);
   --  Register a DSM manager factory for package Pkg_name

private

   type DSM_Manager_Type is abstract
     new Shared_Data_Manager_Type with null record;

end PolyORB.DSA_P.Storages.DSM;
