------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . O B J _ A D A P T E R S . S I M P L E           --
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

--  Object adapters: entities that manage the association
--  of references with servants.

--  $Id$

with PolyORB.Sequences.Unbounded;

with PolyORB.Soft_Links;

package PolyORB.Obj_Adapters.Simple is

   pragma Elaborate_Body;

   type Simple_Obj_Adapter is new Obj_Adapter with private;

   procedure Create (OA : access Simple_Obj_Adapter);

   procedure Destroy (OA : access Simple_Obj_Adapter);

   function Export
     (OA  : access Simple_Obj_Adapter;
      Obj : Objects.Servant_Access)
     return Object_Id;

   procedure Unexport
     (OA : access Simple_Obj_Adapter;
      Id : Object_Id);

   --  In the Simple Object Adapter, the methods of an object
   --  are described using two factory functions (provided by
   --  the application layer) that construct an argument list
   --  and a result Any for a given method.

   type Parameter_Profile_Description is
     access function (Method : Requests.Operation_Id)
     return Any.NVList.Ref;

   type Result_Profile_Description is
     access function (Method : Requests.Operation_Id)
     return Any.Any;

   type Interface_Description is record
      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   procedure Set_Interface_Description
     (OA      : in out Simple_Obj_Adapter;
      Id      : access Object_Id;
      If_Desc : Interface_Description);

   function Get_Empty_Arg_List
     (OA     : access Simple_Obj_Adapter;
      Oid    : access Object_Id;
      Method : Requests.Operation_Id)
     return Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Simple_Obj_Adapter;
      Oid    : access Object_Id;
      Method : Requests.Operation_Id)
     return Any.Any;

   function Find_Servant
     (OA : access Simple_Obj_Adapter;
      Id : access Object_Id)
     return Servant_Access;

   procedure Release_Servant
     (OA : access Simple_Obj_Adapter;
      Id : access Object_Id;
      Servant : in out Servant_Access);

private

   type Object_Map_Entry is record
      --  The Object_Id is simply the position of the
      --  object within the object map.
      Servant : Servant_Access;
      --  May be null (empty entries).

      If_Desc : Interface_Description;
   end record;

   package Object_Map_Entry_Seqs is new PolyORB.Sequences.Unbounded
     (Object_Map_Entry);
   subtype Object_Map_Entry_Seq is Object_Map_Entry_Seqs.Sequence;

   type Simple_Obj_Adapter is new Obj_Adapter with record
      Object_Map : Object_Map_Entry_Seq;
      Lock : Soft_Links.Adv_Mutex_Access;
   end record;

end PolyORB.Obj_Adapters.Simple;
