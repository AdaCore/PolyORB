--  Base POA Manager concrete implementation.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.POA_Types; use PolyORB.POA_Types;
with PolyORB.Locks;
with PolyORB.Components;
with Locked_Queue;
pragma Elaborate_All (Locked_Queue);

package PolyORB.POA_Manager.Basic_Manager is

   pragma Elaborate_Body;

   type Basic_POA_Manager is new POAManager with private;
   type Basic_POA_Manager_Access is access all Basic_POA_Manager;

   Invalid_Obj_Adapter : exception
     renames PolyORB.POA_Manager.Invalid_Obj_Adapter;

   ----------------------------------------------------------------------
   --  Procedures and functions to implement the POAManager interface  --
   ----------------------------------------------------------------------

   procedure Activate
     (Self : access Basic_POA_Manager);

   procedure Hold_Requests
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

   procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access);

   procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access);

   function Get_Hold_Servant
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
     return Hold_Servant_Base_Access;

   ---------------------------------------------------
   --  Servant used to implement the holding state  --
   ---------------------------------------------------

   --  When the POAManager is in the HOLDING state:
   --    A new entry to the queue is created, along with a Hold_Servant
   --    that has access to this entry. The servant is returned to the POA,
   --    which returns it as the requested servant.
   --    When the Handle_Message method of the servant is called, the
   --    Hold_Servant queues the request in the POAManager queue.
   --  When the POAManager changes again to the ACTIVE state:
   --    The requests in the queue are re-sent to the POA, that will send them
   --    to the ORB to be executed again.
   --    Beware that the requests are queued in the ORB queue, and are not
   --    first.

   type Queue_Element_Access is private;
   type Hold_Servant is new Hold_Servant_Base with private;
   type Hold_Servant_Access is access all Hold_Servant;

   function "="
     (Left, Right : Hold_Servant)
     return Boolean;

   function Handle_Message
     (Obj : access Hold_Servant;
      Msg :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

private
   Queue_Size : constant Positive := 10;
   --  The size max of the queue

   type Queue_Element is
      record
         OA  : Obj_Adapter_Access;
         --         Msg : PolyORB.Components.Message;
         --  ??? How do we queue de messages?
      end record;
   type Queue_Element_Access is access all Queue_Element;

   package Requests_Queue_P is new Locked_Queue (Queue_Element_Access);
   subtype Requests_Queue is Requests_Queue_P.Queue;

   type Basic_POA_Manager is new POAManager with
      record
         Usage_Count     : Integer := 0;
         Holded_Requests : Requests_Queue;

         State_Lock      : PolyORB.Locks.Rw_Lock_Access;
         --  Lock the state
         Count_Lock      : PolyORB.Locks.Rw_Lock_Access;
         --  Lock on the usage counter
         POAs_Lock       : PolyORB.Locks.Rw_Lock_Access;
         --  Lock on the sequence of managed POAs
         Queue_Lock      : PolyORB.Locks.Rw_Lock_Access;
         --  Lock on the queue of pending requests
      end record;

   procedure Inc_Usage_Counter (Self : access Basic_POA_Manager);

   procedure Dec_Usage_Counter (Self : access Basic_POA_Manager);

   type Hold_Servant is new Hold_Servant_Base with
      record
         Queue_Entry : Queue_Element_Access;
      end record;

   procedure Create
     (HS  : in out Hold_Servant;
      QEA : in Queue_Element_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_POA_Manager, Basic_POA_Manager_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Hold_Servant, Hold_Servant_Access);

end PolyORB.POA_Manager.Basic_Manager;


