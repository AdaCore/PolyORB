------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
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

with PolyORB.Any;
with PolyORB.Any.NVList;

with PolyORB.Components;
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

package PolyORB.Obj_Adapters is

   type Obj_Adapter is abstract new Smart_Pointers.Entity
     with private;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   procedure Create (OA : access Obj_Adapter) is abstract;
   --  Initialize.

   procedure Destroy (OA : access Obj_Adapter) is abstract;
   --  Finalize.

   Invalid_Object_Id : exception;
   --  An invalid object identifier was passed to an object
   --  adapter subprogram.

   Invalid_Method : exception;
   --  A method was invoked on an object that does not implement it.

   procedure Set_ORB
     (OA      : access Obj_Adapter;
      The_ORB :        Components.Component_Access);
   --  Set the ORB whose OA is attached to.

   --------------------------------------
   -- Interface to application objects --
   --------------------------------------

   function Export
     (OA  : access Obj_Adapter;
      Obj :        Objects.Servant_Access;
      Key :        Objects.Object_Id_Access := null)
      return Objects.Object_Id
      is abstract;
   --  Create an identifier for Obj within OA. If Key is
   --  not null, use it as an application-level identifier
   --  for the object (which will be used to construct the
   --  local identifier).

   procedure Unexport
     (OA : access Obj_Adapter;
      Id :        Objects.Object_Id_Access)
      is abstract;
   --  Id is an object identifier attributed by OA.
   --  The corresponding association is suppressed.

   function Object_Key
     (OA : access Obj_Adapter;
      Id :        Objects.Object_Id_Access)
      return Objects.Object_Id
      is abstract;
   --  Return the application-level identifier associated
   --  with Id.

   ----------------------------------------------------
   -- Interface to ORB (acting on behalf of clients) --
   ----------------------------------------------------

   function Get_Empty_Arg_List
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method : Requests.Operation_Id)
      return Any.NVList.Ref
      is abstract;
   --  Return the paramter profile of the given method, so the
   --  protocol layer can unmarshall the message into a Request object.

   function Get_Empty_Result
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method : Requests.Operation_Id)
      return Any.Any
      is abstract;
   --  Return the result profile of the given method.

   function Find_Servant
     (OA : access Obj_Adapter;
      Id : access Objects.Object_Id)
      return Objects.Servant_Access
      is abstract;
   --  Retrieve the servant managed by OA for logical object Id.
   --  The servant that incarnates the object is return.

   procedure Release_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Objects.Servant_Access)
      is abstract;
   --  Signal to OA that a Servant previously obtained using
   --  Find_Servant won't be used by the client anymore. This
   --  may cause the servant to be destroyed if so is OA's
   --  policy.

   ----------------------------------
   -- Export of object identifiers --
   ----------------------------------

   function Oid_To_Rel_URI
     (OA : access Obj_Adapter;
      Id : access Objects.Object_Id)
     return Types.String;

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : Types.String)
     return Objects.Object_Id_Access;

   --  Convert an object id from/to its representation as
   --  a relative URI. A default implementation of these
   --  functions is provided; actual object adapters may
   --  overload them if desired.

   --------------------------------
   -- Proxy namespace management --
   --------------------------------

   --  The object id name space is managed entirely by the
   --  object adapter. Consequently, the OA is also responsible
   --  for assigning object IDs to virtual proxy objects
   --  corresponding to object references for which we act as
   --  a proxy.

   function Is_Proxy_Oid
     (OA  : access Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean;

   function To_Proxy_Oid
     (OA : access Obj_Adapter;
      R  :        References.Ref)
     return Objects.Object_Id_Access;

   function Proxy_To_Ref
     (OA  : access Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref;

   --  These operations may be left unimplemented by some object
   --  adapter types, in which case the above two operations
   --  shall raise PolyORB.Not_Implemented.

private

   type Obj_Adapter is abstract new Smart_Pointers.Entity with
      record
         ORB : Components.Component_Access;
         --  The ORB the OA is attached to. Needs to be casted into an
         --  ORB_Access when used.
      end record;

end PolyORB.Obj_Adapters;
