------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P O A . B A S I C _ P O A                 --
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

--  Basic POA implementation.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Objects;
with PolyORB.POA_Policies;
with PolyORB.References;
with PolyORB.Requests;

package PolyORB.POA.Basic_POA is

   pragma Elaborate_Body;

   type Basic_Obj_Adapter is new PolyORB.POA.Obj_Adapter
     with private;
   type Basic_Obj_Adapter_Access is access all Basic_Obj_Adapter;
   --  The POA object

   --------------------------------------------------
   --  Procedures and functions required by CORBA  --
   --------------------------------------------------

   function Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        PolyORB.POA_Policies.PolicyList)
     return Obj_Adapter_Access;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

   procedure Destroy
     (Self                : access Basic_Obj_Adapter;
      Etherealize_Objects : in     Boolean;
      Wait_For_Completion : in     Boolean);

   function Create_Object_Identification
     (Self : access Basic_Obj_Adapter;
      Hint :        Object_Id_Access := null)
     return Unmarshalled_Oid;

   function Activate_Object
     (Self      : access Basic_Obj_Adapter;
      P_Servant :        Objects.Servant_Access;
      Hint      :        Object_Id_Access := null)
     return Object_Id;

   procedure Deactivate_Object
     (Self      : access Basic_Obj_Adapter;
      Oid       : in Object_Id);

   function Servant_To_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Objects.Servant_Access)
     return Object_Id;

   function Id_To_Servant
     (Self : access Basic_Obj_Adapter;
      Oid  :        Object_Id)
     return Objects.Servant_Access;

   --------------------------------------------------------
   --  Functions and procedures to interface with PolyORB --
   --------------------------------------------------------

   procedure Create
     (OA : access Basic_Obj_Adapter);

   procedure Destroy
     (OA : access Basic_Obj_Adapter);

   function Export
     (OA  : access Basic_Obj_Adapter;
      Obj :        PolyORB.Objects.Servant_Access)
     return PolyORB.Objects.Object_Id;

   procedure Unexport
     (OA : access Basic_Obj_Adapter;
      Id :        PolyORB.Objects.Object_Id);

   function Get_Empty_Arg_List
     (OA     : access Basic_Obj_Adapter;
      Oid    : access PolyORB.Objects.Object_Id;
      Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Basic_Obj_Adapter;
      Oid    : access PolyORB.Objects.Object_Id;
      Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.Any;

   function Find_Servant
     (OA : access Basic_Obj_Adapter;
      Id : access PolyORB.Objects.Object_Id)
     return PolyORB.Objects.Servant_Access;

   procedure Release_Servant
     (OA      : access Basic_Obj_Adapter;
      Id      : access PolyORB.Objects.Object_Id;
      Servant : in out PolyORB.Objects.Servant_Access);

   -------------------------------------------------
   --  Utilities, neither in CORBA nor in PolyORB  --
   -------------------------------------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_Obj_Adapter;
      To   : access Basic_Obj_Adapter);

   procedure Remove_POA_By_Name
     (Self       : access Basic_Obj_Adapter;
      Child_Name :        Types.String);
   --  Remove a child POA from Self's list of children
   --  Doesn't lock the list of children

   --------------------------------
   -- Proxy namespace management --
   --------------------------------

   procedure Set_Proxies_OA
     (OA         : access Basic_Obj_Adapter;
      Proxies_OA :        Basic_Obj_Adapter_Access);

   function Is_Proxy_Oid
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean;

   function To_Proxy_Oid
     (OA : access Basic_Obj_Adapter;
      R  :        References.Ref)
     return Object_Id_Access;

   function Proxy_To_Ref
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref;

private

   type Basic_Obj_Adapter is new PolyORB.POA.Obj_Adapter with record
      Proxies_OA : Basic_Obj_Adapter_Access;
      --  The child POA used for management of the proxy objects
      --  namespace (used only in the Root POA instance.)
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Obj_Adapter, Basic_Obj_Adapter_Access);

   type Check_State is (CHECK, NO_CHECK);

end PolyORB.POA.Basic_POA;
