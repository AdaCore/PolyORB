------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P O A . B A S I C _ P O A                 --
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

--  Basic POA implementation.

--  As an implementation package of abstract functions defined in
--  PolyORB.POA, this package provides an implementation for both the
--  CORBA-like POA API (defined in PolyORB.POA) and PolyORB Obj_Adapter
--  (defined in PolyORB.Obj_Adapters) upon which the Basic POA depends.

with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.References;

package PolyORB.POA.Basic_POA is

   pragma Elaborate_Body;

   type Basic_Obj_Adapter is new POA.Obj_Adapter with private;
   type Basic_Obj_Adapter_Access is access all Basic_Obj_Adapter;
   --  The POA object

   procedure Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Standard.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container);

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

   procedure To_Proxy_Oid
     (OA    : access Basic_Obj_Adapter;
      R     :        References.Ref;
      Oid   :    out Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Proxy_To_Ref
     (OA    : access Basic_Obj_Adapter;
      Oid   : access Objects.Object_Id;
      Ref   : out References.Ref;
      Error : in out PolyORB.Errors.Error_Container);

private

   type Basic_Obj_Adapter is new POA.Obj_Adapter with record
      Proxies_OA : Basic_Obj_Adapter_Access;
      --  The child POA used for management of the proxy objects
      --  namespace (used only in the Root POA instance.)
   end record;

end PolyORB.POA.Basic_POA;
