--  Abstract interface for the POA manager.

--  $Id$

with Droopi.POA_Types; use Droopi.POA_Types;
with Droopi.Smart_Pointers;

package Droopi.POA_Manager is

   --  Unit has no proper body: no elab control necessary.

   type State is
     (HOLDING, ACTIVE, DISCARDING, INACTIVE);

   type POAManager is abstract new Smart_Pointers.Entity
     with private;

   type POAManager_Access is access all POAManager'Class;

   subtype POAManager_Object_Ptr is POAManager_Access;
   --  XXX for easier portability of legacy AdaBroker code.

   Invalid_Obj_Adapter : exception;

   type Hold_Servant_Base is abstract new Droopi.POA_Types.Servant
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

   type POAManager is abstract new Smart_Pointers.Entity with record
      Current_State : State;
      Managed_POAs  : POAList_Access;
   end record;

end Droopi.POA_Manager;
