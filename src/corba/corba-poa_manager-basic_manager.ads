with Ada.Unchecked_Deallocation;

with CORBA.POA_Types; use CORBA.POA_Types;
with Droopi.Locks;

package CORBA.POA_Manager.Basic_Manager is

   type Basic_POA_Manager is new POAManager with private;
   type Basic_POA_Manager_Access is access all Basic_POA_Manager;

   Invalid_Obj_Adapter : exception
     renames CORBA.POA_Manager.Invalid_Obj_Adapter;

   ----------------------------------------------------------------------
   --  Procedures and functions to implement the POAManager interface  --
   ----------------------------------------------------------------------

   procedure Activate
     (Self : access Basic_POA_Manager);

   procedure Hold_Request
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean);

   procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean);

   procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean);

   function Get_State
     (Self : Basic_POA_Manager)
     return State;

   ---------------------------------------------------------------
   --  Procedures and functions specific to the implementation  --
   ---------------------------------------------------------------

   procedure Create (M : access Basic_POA_Manager);

   procedure Destroy (M : access Basic_POA_Manager);

   procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access);

   procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access);

private
   type Basic_POA_Manager is new POAManager with record
      Usage_Count : Integer := 0;
      State_Lock  : Droopi.Locks.Rw_Lock_Access;
      --  Lock the state
      Count_Lock  : Droopi.Locks.Rw_Lock_Access;
      --  Lock on the usage counter
      POAs_Lock   : Droopi.Locks.Rw_Lock_Access;
      --  Lock on the sequence of managed POAs
   end record;

   procedure Inc_Usage_Counter (Self : access Basic_POA_Manager);

   procedure Dec_Usage_Counter (Self : access Basic_POA_Manager);

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_POA_Manager, Basic_POA_Manager_Access);

end CORBA.POA_Manager.Basic_Manager;


