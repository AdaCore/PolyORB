with Ada.Exceptions;                      use Ada.Exceptions;

with Interfaces;                          use Interfaces;
with Interfaces.C;

with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Priorities;
with System.Garlic.Soft_Links;            use System.Garlic.Soft_Links;
with System.Garlic.Protocols.Tcp;         use System.Garlic.Protocols.Tcp;
with System.Garlic.Types;                 use System.Garlic.Types;

package body System.Garlic.Protocols.Tcp.Server is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GPTCSE", "(s-gptcse): ");

   use type C.int;

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   procedure Allocate_Connector
     (Peer : in Interfaces.C.int;
      PID  : in Types.Partition_ID);

   procedure Allocate_Acceptor
     (Incoming : in Natural);

   task type Accept_Handler is
      pragma Priority (Priorities.RPC_Priority);
      entry Initialize (My_Index : in Natural);
   end Accept_Handler;
   type Accept_Access is access Accept_Handler;
   --  Accept new connections. Initialize indicates the index of
   --  incoming connection in Incomings table. Remember that this
   --  protocol may support several self locations or connections.

   type Connect_Record;
   type Connect_Access is access Connect_Record;
   task type Connect_Handler is
      pragma Priority (Priorities.RPC_Priority);
      entry Initialize
        (My_Peer : C.int;
         My_PID  : Partition_ID;
         My_Self : Connect_Access);
   end Connect_Handler;
   type Connect_Handler_Access is access Connect_Handler;
   type Connect_Record is record
      Next : Connect_Access;
      Self : Connect_Handler_Access;
   end record;
   --  Handle incoming connection

   procedure Dequeue_Connector (Connector : in out Connect_Access);
   procedure Enqueue_Connector (Connector : in out Connect_Access);
   Connect_List : Connect_Access;

   --------------------
   -- Accept_Handler --
   --------------------

   task body Accept_Handler is
      Incoming : Natural;

   begin
      select
         accept Initialize (My_Index : in Natural) do
            Incoming := My_Index;
         end Initialize;
      or
         terminate;
      end select;
      pragma Debug (D ("Task Accept Handler is running"));

      Accept_Until_Closed (Incoming);

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D ("Accept Handler: " & Exception_Name (E)));
         pragma Debug (D ("Accept Handler: " & Exception_Information (E)));
         null;
   end Accept_Handler;

   -----------------------
   -- Allocate_Acceptor --
   -----------------------

   procedure Allocate_Acceptor
     (Incoming : in Natural)
   is
      Acceptor : Accept_Access;

   begin
      Acceptor := new Accept_Handler;
      Acceptor.Initialize (Incoming);
   end Allocate_Acceptor;

   ------------------------
   -- Allocate_Connector --
   ------------------------

   procedure Allocate_Connector
     (Peer : in Interfaces.C.int;
      PID  : in Types.Partition_ID)
   is
      Connector : Connect_Access;

   begin
      Dequeue_Connector (Connector);
      Connector.Self.Initialize (Peer, PID, Connector);
   end Allocate_Connector;

   ---------------------
   -- Connect_Handler --
   ---------------------

   task body Connect_Handler is
      Self   : Connect_Access;
      PID    : Partition_ID;
      Peer   : C.int;

   begin
      loop
         select
            accept Initialize
              (My_Peer : C.int;
               My_PID  : Partition_ID;
               My_Self : Connect_Access)
            do
               Peer := My_Peer;
               PID  := My_PID;
               Self := My_Self;
            end Initialize;
         or
            terminate;
         end select;

         pragma Debug (D ("Task Connector Handler is running"));

         Receive_Until_Closed (Peer, PID);
         Enqueue_Connector (Self);
      end loop;

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D ("Connector Handler: " & Exception_Name (E)));
         pragma Debug (D ("Connector Handler: " & Exception_Information (E)));
         null;
   end Connect_Handler;

   -----------------------
   -- Dequeue_Connector --
   -----------------------

   procedure Dequeue_Connector
     (Connector : in out Connect_Access) is
   begin
      Enter_Critical_Section;
      if Connect_List = null then
         pragma Debug (D ("Create a new connection handler"));
         Connector      := new Connect_Record;
         Connector.Self := new Connect_Handler;
      else
         pragma Debug (D ("Reuse an old connection handler"));
         Connector      := Connect_List;
         Connect_List := Connector.Next;
      end if;
      Leave_Critical_Section;
   end Dequeue_Connector;

   ------------------------
   -- Enqueue_Connector --
   ------------------------

   procedure Enqueue_Connector
     (Connector : in out Connect_Access) is
   begin
      pragma Debug (D ("Queue an old connection handler"));
      Enter_Critical_Section;
      Connector.Next := Connect_List;
      Connect_List := Connector;
      Leave_Critical_Section;
   end Enqueue_Connector;

begin
   Register_Task_Pool
     (Allocate_Acceptor'Access,
      Allocate_Connector'Access);
end System.Garlic.Protocols.Tcp.Server;
