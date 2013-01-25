------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  This package provides the root definition of all Object adapters.
--  An Object Adapter manages the association of references to servants.

with PolyORB.Annotations;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.QoS;
with PolyORB.References;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

package PolyORB.Obj_Adapters is

   type Obj_Adapter is abstract
     new Smart_Pointers.Non_Controlled_Entity with private;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   procedure Create (OA : access Obj_Adapter) is abstract;
   --  Set up OA's internal structures

   procedure Destroy (OA : access Obj_Adapter);
   --  Deallocate OA's internal structures

   overriding procedure Finalize (OA : in out Obj_Adapter);
   --  Makes a dispatching call on Destroy

   --------------------------------------
   -- Interface to application objects --
   --------------------------------------

   procedure Export
     (OA    : access Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Create an identifier for Obj within OA. If Key is not null, use it as
   --  an application-level identifier for the object (which will be used to
   --  construct the local identifier).

   procedure Unexport
     (OA    : access Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Id is an object identifier attributed by OA. The corresponding
   --  association is suppressed.

   procedure Object_Key
     (OA      : access Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  If Id is user defined associated with Id, return user identifier
   --  component of Id, else raise an error.

   procedure Get_QoS
     (OA    : access Obj_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Return the QoS information managed by object adapter OA, for
   --  object denoted by Id.

   ----------------------------------------------------
   -- Interface to ORB (acting on behalf of clients) --
   ----------------------------------------------------

   function Get_Empty_Arg_List
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method : String)
      return Any.NVList.Ref
      is abstract;
   --  Return the parameter profile of the given method, so the protocol layer
   --  can unmarshall the message into a Request object.

   function Get_Empty_Result
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method : String)
      return Any.Any
      is abstract;
   --  Return the result profile of the given method

   procedure Find_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Retrieve the servant managed by OA for logical object Id. The servant
   --  that incarnates the object is returned.

   procedure Release_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access)
      is abstract;
   --  Signal to OA that a Servant previously obtained using Find_Servant won't
   --  be used by the client anymore. This may cause the servant to be
   --  destroyed if so is OA's policy.

   ----------------------------------
   -- Export of object identifiers --
   ----------------------------------

   procedure Oid_To_Rel_URI
     (OA    : access Obj_Adapter;
      Id    : access Objects.Object_Id;
      URI   : out Types.String;
      Error : in out PolyORB.Errors.Error_Container);

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : String) return Objects.Object_Id_Access;

   --  Convert an object id from/to its representation as a relative URI. A
   --  default implementation of these functions is provided; actual object
   --  adapters may overload them if desired.

   --------------------------------
   -- Proxy namespace management --
   --------------------------------

   --  The object id name space is managed entirely by the object adapter.
   --  Consequently, the OA is also responsible for assigning object IDs to
   --  virtual proxy objects corresponding to object references for which we
   --  act as a proxy.

   function Is_Proxy_Oid
     (OA  : access Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean;
   --  Determine whether Oid is the identifier for a proxy object. Always
   --  False if OA does not support proxy objects.

   procedure To_Proxy_Oid
     (OA    : access Obj_Adapter;
      R     : References.Ref;
      Oid   : out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);
   --  Create a proxy oid for reference R. No_Implement_E is thrown if OA
   --  does not support proxy objects.

   procedure Proxy_To_Ref
     (OA    : access Obj_Adapter;
      Oid   : access Objects.Object_Id;
      Ref   : out References.Ref;
      Error : in out PolyORB.Errors.Error_Container);
   --  Retrieve the reference for which Oid is a proxy oid into Ref.
   --  No_Implement_E is thrown if OA does not support proxy objects.

   ----------------------------
   -- Annotations management --
   ----------------------------

   function Notepad_Of
     (OA : access Obj_Adapter)
     return Annotations.Notepad_Access;

private

   type Obj_Adapter is abstract new Smart_Pointers.Non_Controlled_Entity with
      record
         Notepad : aliased Annotations.Notepad;
         --  OA's notepad. The user is responsible for ensuring protection
         --  against incorrect concurrent accesses.
      end record;

end PolyORB.Obj_Adapters;
