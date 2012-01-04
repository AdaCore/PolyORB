------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . P O A _ M A N A G E R                   --
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

--  Abstract interface for the POA manager.

with PolyORB.Errors;
with PolyORB.Servants;
with PolyORB.POA_Types;

with PolyORB.Smart_Pointers;

package PolyORB.POA_Manager is

   use PolyORB.POA_Types;

   --  Unit has no proper body: no elab control necessary.

   type State is (HOLDING, ACTIVE, DISCARDING, INACTIVE);

   type Ref is new Smart_Pointers.Ref with private;

   type POAManager is abstract new Smart_Pointers.Non_Controlled_Entity
     with private;

   type POAManager_Access is access all POAManager'Class;

   --------------------------------------------------------------------
   -- Procedures and functions to implement the POAManager interface --
   --------------------------------------------------------------------

   procedure Activate
     (Self  : access POAManager;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;

   procedure Hold_Requests
     (Self                : access POAManager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Errors.Error_Container)
     is abstract;

   procedure Discard_Requests
     (Self                : access POAManager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Errors.Error_Container)
      is abstract;

   procedure Deactivate
     (Self                : access POAManager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean)
     is abstract;

   function Get_State (Self : POAManager) return State is abstract;

   -------------------------------------------------------------
   -- Procedures and functions specific to the implementation --
   -------------------------------------------------------------

   procedure Create (M : access POAManager) is abstract;

   procedure Register_POA
     (Self : access POAManager;
      OA   :        Obj_Adapter_Access)
      is abstract;

   procedure Remove_POA
     (Self : access POAManager;
      OA   :        Obj_Adapter_Access)
      is abstract;

   function Get_Hold_Servant
     (Self : access POAManager;
      OA   :        Obj_Adapter_Access)
     return PolyORB.Servants.Servant_Access
      is abstract;

private

   type Ref is new Smart_Pointers.Ref with null record;

   type POAManager is abstract new Smart_Pointers.Non_Controlled_Entity
      with null record;

end PolyORB.POA_Manager;
