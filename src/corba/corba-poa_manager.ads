with CORBA.POA_Types; use CORBA.POA_Types;

package CORBA.POA_Manager is

   type State is
     (HOLDING, ACTIVE, DISCARDING, INACTIVE);

   type POAManager is abstract tagged private;
   type POAManager_Access is access all POAManager'Class;

   Invalid_Obj_Adapter : exception;

   ----------------------------------------------------------------------
   --  Procedures and functions to implement the POAManager interface  --
   ----------------------------------------------------------------------

   procedure Activate
     (Self : access POAManager)
      is abstract;

   procedure Hold_Request
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

   procedure Destroy
     (M : access POAManager)
     is abstract;

   procedure Register_POA
     (Self : access POAManager;
      OA   : Obj_Adapter_Access)
      is abstract;

   procedure Remove_POA
     (Self : access POAManager;
      OA   : Obj_Adapter_Access)
     is abstract;

private
   type POAManager is abstract tagged record
      Current_State : State;
      Managed_POAs  : POAList_Access;
   end record;

end CORBA.POA_Manager;
