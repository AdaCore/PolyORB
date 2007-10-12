------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.REQUEST_PROCESSING_POLICY               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Errors;
with PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.POA_Policies.Request_Processing_Policy is

   use PolyORB.POA_Types;

   type RequestProcessingPolicy is abstract new Policy with null record;

   type RequestProcessingPolicy_Access is
     access all RequestProcessingPolicy'Class;

   procedure Id_To_Servant
     (Self    :        RequestProcessingPolicy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Case USE_OBJECT_MAP_ONLY:
   --    Asks the Servant Retention Policy to look for the given Oid.
   --    If NON_RETAIN, raises WrongPolicy.
   --    If found, returns the associated servant.
   --    Otherwise, raises ObjectNotActive
   --  Case USE_DEFAULT_SERVANT:
   --    If not found in the Active Object Map, returns the default servant.
   --    If there's no default servant registered, raises Obj_Adapter with
   --    minor code 3.
   --  Case USE_SERVANT_MANAGER:
   --    If not found in the Active Object Map, asks the servant manager
   --    to create it. If there's not servant manager, raises Obj_Adapter
   --    with minor code 4.

   procedure Set_Servant
     (Self    :        RequestProcessingPolicy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;

   procedure Get_Servant
     (Self    :        RequestProcessingPolicy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;

   procedure Ensure_Servant_Manager
     (Self  :        RequestProcessingPolicy;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;

end PolyORB.POA_Policies.Request_Processing_Policy;
