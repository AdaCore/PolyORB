------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . P O A _ M A N A G E R                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Abstract interface for the POA manager.

--  $Id$

with PolyORB.POA_Types; use PolyORB.POA_Types;
with PolyORB.Smart_Pointers;

package PolyORB.POA_Manager is

   --  Unit has no proper body: no elab control necessary.

   type State is
     (HOLDING, ACTIVE, DISCARDING, INACTIVE);

   type Ref is new Smart_Pointers.Ref with private;

   type POAManager is abstract new Smart_Pointers.Entity
     with private;

   type POAManager_Access is access all POAManager'Class;

   subtype POAManager_Object_Ptr is POAManager_Access;
   --  XXX for easier portability of legacy AdaBroker code.

   Invalid_Obj_Adapter : exception;

   type Hold_Servant_Base is abstract new PolyORB.POA_Types.Servant
     with null record;
   type Hold_Servant_Base_Access is
     access all Hold_Servant_Base'Class;

   ----------------------------------------------------------------------
   --  Procedures and functions to implement the POAManager interface  --
   ----------------------------------------------------------------------

   procedure Activate
     (Self : access POAManager)
      is abstract;

   procedure Hold_Requests
     (Self                : access POAManager;
      Wait_For_Completion :        Boolean)
     is abstract;

   procedure Discard_Requests
     (Self                : access POAManager;
      Wait_For_Completion :        Boolean)
      is abstract;

   procedure Deactivate
     (Self                : access POAManager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean)
     is abstract;

   function Get_State
     (Self : POAManager)
     return State
      is abstract;

   ---------------------------------------------------------------
   --  Procedures and functions specific to the implementation  --
   ---------------------------------------------------------------

   procedure Create
     (M : access POAManager)
      is abstract;

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
     return Hold_Servant_Base_Access
      is abstract;

private

   type Ref is new Smart_Pointers.Ref with null record;

   type POAManager is abstract new Smart_Pointers.Entity with record
      Current_State : State;
      Managed_POAs  : POAList_Access;
   end record;

end PolyORB.POA_Manager;
