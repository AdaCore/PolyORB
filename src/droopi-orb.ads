--  The ORB main loop and scheduler.

--  $Id$

with Sequences.Unbounded;

with Droopi.Jobs;
with Droopi.Protocols;
with Droopi.Soft_Links;
with Droopi.Sockets;

package Droopi.ORB is

   ---------------------
   -- A server object --
   ---------------------

   type Root_ORB is abstract tagged limited private;
   type ORB_Access is access all Root_ORB'Class;

   ------------------------
   -- A decorated socket --
   ------------------------

   type Socket_Kind is
     (Invalid_SK,
      Listening_SK,
      Communication_SK);

   type Active_Socket (Kind : Socket_Kind := Invalid_SK) is record
      Socket   : Sockets.Socket_Type;
      Protocol : Protocols.Protocol_Access;

      case Kind is
         when Communication_Sk =>
            Session  : Protocols.Session_Access;
         when others =>
            null;
      end case;
   end record;

   procedure Handle_Event (O : access Root_ORB; AS : Active_Socket)
     is abstract;
   --  Process events that have occurred on active socket AS, managed
   --  by server O.

   package Sock_Seqs is new Sequences.Unbounded (Active_Socket);
   subtype Sock_Seq is Sock_Seqs.Sequence;

   ------------------------------
   -- Server object operations --
   ------------------------------

   function Create_ORB return ORB_Access;
   --  Create a new ORB and initialize it.

   type Exit_Condition_Access is access all Boolean;

   procedure Run
     (O              : access Root_ORB;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False) is abstract;
   --  Execute the ORB until:
   --    - Exit_Condition.all becomes true
   --      (if Exit_Condition /= null), or
   --    - Shutdown is called on this ORB.

   --  This is executed by ORB tasks (with Exit_Condition = null)
   --  and is entered by user tasks that need to wait for an event
   --  to occur in the ORB (such tasks must execute the ORB when
   --  the threading policy is 'no threads').

   --  If May_Poll, then this task may suspend itself to wait
   --  for external events.

   function Work_Pending (O : access Root_ORB) return Boolean
     is abstract;
   --  Return True if, and only if, some Root_ORB processing is
   --  pending.

   procedure Perform_Work (O : access Root_ORB) is abstract;
   --  Perform one elementary Root_ORB action and return.

   procedure Shutdown
     (O                   : access Root_ORB;
      Wait_For_Completion : Boolean := True) is abstract;
   --  Shut down Root_ORB O. If Wait_For_Completion is True, do
   --  not return before the shutdown is completed.

   procedure Insert_Socket
     (O  : access Root_ORB;
      AS : Active_Socket) is abstract;
   --  Insert socket S with kind K in the set of sockets monitored by O.

   procedure Delete_Socket
     (O : access Root_ORB;
      S : Sockets.Socket_Type) is abstract;
   --  Delete socket S from the set of sockets monitored by O.

private

   type Root_ORB is abstract tagged limited record

      ------------------
      -- Server state --
      ------------------

      Shutdown   : Boolean := False;
      --  Set to True when Root_ORB shutdown has been requested.

      Job_Queue  : Jobs.Job_Queue_Access;
      --  The queue of jobs to be processed by Root_ORB tasks.

      Idle_Tasks : Soft_Links.Barrier_Access;
      --  Idle Root_ORB task wait on this barrier.

      ORB_Sockets : Sock_Seq;
      --  The set of transport endpoints to be monitored
      --  by Root_ORB tasks.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on Monitored_Sockets.

      Selector : Sockets.Selector_Access;
      --  The selector object used to wait for an external event.

   end record;

end Droopi.ORB;
