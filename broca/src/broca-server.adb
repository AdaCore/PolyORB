with System;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Text_IO;
with CORBA; use CORBA;
with PortableServer;
with Broca.Types; use Broca.Types;
with Broca.Exceptions;
with Broca.Marshalling;
with Broca.Giop;
with Broca.Orb;
with Broca.Stream;
with Broca.Flags;
pragma Elaborate_All (Broca.Orb);

package body Broca.Server is
   --  Just disp a string as a log message.
   --  FIXME: should go somewhere else.
   procedure Log (S : String) is
      use Ada.Text_IO;
   begin
      if Broca.Flags.Log then
         Put ("ORB log: ");
         Put_Line (S);
      end if;
   end Log;

   --  Table of POAs.
   --
   --  The table contains all POAs created in this server.
   --  The purpose is to provide a fast method to get a poa from
   --  an object_key: the object_key contains an index into this table.
   --  The POA will translate a part of the object_key to object_id or index
   --  in its own table.
   --
   --  To be able to reuse entries in this table or to detect destroyed POAs
   --  a symbolic date is associed to each entry.  Each time an entry is
   --  update (a new POA is added), the date is incremented.
   --  An object_key contains also the date associed with the entry.
   --  If, during decoding an object_key, date mismatches, then either
   --  the POA does not exist anymore, or it was moved.  The latest case is
   --  possible only for PERSISTENT POAs.  In this case, the full POA name
   --  is contained in the object_key and can be used to find the new entry
   --  (or to create a POA). To avoid to decode the full reference, a
   --  locationForward message can be sent.
   type POA_Entry_Type is
      record
         Poa : Broca.Poa.POA_Object_Access := null;
         --  The date is incremented during unregister.
         --  FIXME: Overflow is not handled
         Date : Natural := 0;
      end record;

   type POA_Entry_Array is array (Broca.Poa.Poa_Index_Type range <>)
     of POA_Entry_Type;
   type POA_Entry_Array_Acc is access POA_Entry_Array;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => POA_Entry_Array, Name => POA_Entry_Array_Acc);

   --  This is the variable containing an access to the table of objects.
   All_POAs : POA_Entry_Array_Acc := null;

   --  Find a free entry in the table of objects or expand it.
   procedure Register_POA (Poa : Broca.Poa.POA_Object_Access) is
      use type Broca.Poa.POA_Object_Access;
   begin
      Log ("register_poa: " & CORBA.To_Standard_String (Poa.Name));
      if All_POAs = null then
         --  The table was never built.
         All_POAs := new POA_Entry_Array (1 .. 16);
         All_POAs (1) := (Poa => Poa, Date => 1);
         Poa.Index := 1;
         return;
      else
         --  Try to find a free entry.
         --  FIXME: optimize here.
         for I in All_POAs.all'Range loop
            if All_POAs (I).Poa = null then
               --  An entry was found.
               All_POAs (I).Poa := Poa;
               Poa.Index := I;
               return;
            end if;
         end loop;

         --  Expand the table.
         declare
            use Broca.Poa;
            N_Ao : POA_Entry_Array_Acc;
            O_Ao : POA_Entry_Array_Acc;
            Slot : Poa_Index_Type;
         begin
            N_Ao := new POA_Entry_Array (1 .. 2 * All_POAs.all'Last);
            N_Ao (All_POAs.all'Range) := All_POAs.all;
            Slot := All_POAs.all'Last + 1;
            N_Ao (Slot) := (Poa => Poa, Date => 1);
            O_Ao := All_POAs;
            All_POAs := N_Ao;
            Unchecked_Deallocation (O_Ao);
            Poa.Index := Slot;
            return;
         end;
      end if;
   end Register_POA;

   procedure Unregister_POA (Poa : Broca.Poa.POA_Object_Access) is
   begin
      All_POAs (Poa.Index) := (Poa => null,
                               Date => All_POAs (Poa.Index).Date + 1);
   end Unregister_POA;

   --  Coding of object key:
   --  An object key is a sequence of octet, whose length LENGTH is known.
   --  0 .. 3: boot date or 0 if persistent POA.
   --  4 .. 7: poa index in all_POAs
   --  8 .. 11: date of the entry
   --  12 .. 15: N_POA
   --           number of POAs in the POA path name, can be 0 if POA is
   --           TRANSIENT.  The POA path name starts after the RootPOA, ie
   --           for an object of the RootPOA, its path name is empty.
   --  alignment on long boundary.
   --  for 1 .. N_POA
   --     0 .. X: poa name, as a CORBA string
   --  ? .. ?: key given by the POA, the length is known.

   --  Marshall procedure are called only by Build_IOR.
   --  The POA must be locked to prevent any destruction.
   procedure Marshall_Size_Object_Key
     (Buffer : in out Buffer_Descriptor; Poa : Broca.Poa.POA_Object_Access);
   procedure Marshall_Object_Key
     (Buffer : in out Buffer_Descriptor; Poa : Broca.Poa.POA_Object_Access);

   --  Decode (unmarshall) an object_key.
   --  POA is the last POA that was reached.  It can be different from the
   --  POA of the object, if a adapter activator has to be called but the
   --  state of the POA associated with the activator was not active.
   --  If POA is an intermediate POA, then KEY.POS is 0.
   --
   --  After the call, POA, if not null, has been link_lock.lock_R.
   --  POA_STATE is the state of the POA.
   procedure Unmarshall_Object_Key
     (Buffer : in out Buffer_Descriptor;
      Poa : out Broca.Poa.POA_Object_Access;
      Poa_State : out Broca.Poa.Processing_State_Type;
      Key : in out Buffer_Descriptor);

   procedure Marshall_Size_Object_Key
     (Buffer : in out Buffer_Descriptor; Poa : Broca.Poa.POA_Object_Access) is
      use Broca.Marshalling;
      use Broca.Poa;
      use PortableServer;
      Current : Broca.Poa.POA_Object_Access;
   begin
      --  Boot date
      Marshall_Size_Unsigned_Long (Buffer);

      --  POA index
      Marshall_Size_Unsigned_Long (Buffer);

      --  Date
      Marshall_Size_Unsigned_Long (Buffer);

      --  Number of poas
      Marshall_Size_Unsigned_Long (Buffer);

      if Poa.Lifespan_Policy = PortableServer.PERSISTENT then
         --  Due to the alignment, the order of POA name is not important.
         Current := Poa;
         while Current.Parent /= null loop
            Marshall_Size (Buffer, Current.Name);
            Current := Current.Parent;
         end loop;
         Marshall_Align_4 (Buffer);
      end if;
   end Marshall_Size_Object_Key;

   --  Create an object_key.
   procedure Marshall_Object_Key
     (Buffer : in out Buffer_Descriptor;
      Poa : Broca.Poa.POA_Object_Access;
      Num : Natural);

   procedure Marshall_Object_Key
     (Buffer : in out Buffer_Descriptor;
      Poa : Broca.Poa.POA_Object_Access;
      Num : Natural)
   is
      use Broca.Marshalling;
      use Broca.Poa;
   begin
      if Poa.Parent = null then
         --  The RootPOA was reached.
         Marshall (Buffer, CORBA.Unsigned_Long (Num));
      else
         Marshall_Object_Key (Buffer, Poa.Parent, Num + 1);
         Marshall (Buffer, Poa.Name);
         if Num = 0 then
            Marshall_Align_4 (Buffer);
         end if;
      end if;
   end Marshall_Object_Key;

   procedure Marshall_Object_Key
     (Buffer : in out Buffer_Descriptor; Poa : Broca.Poa.POA_Object_Access)
   is
      use Broca.Marshalling;
      use PortableServer;
   begin
      if Poa.Lifespan_Policy = PortableServer.TRANSIENT then
         Marshall (Buffer, CORBA.Unsigned_Long (0));
      else
         Marshall (Buffer, Broca.Flags.Boot_Time);
      end if;
      Marshall (Buffer, CORBA.Unsigned_Long (Poa.Index));
      Marshall (Buffer, CORBA.Unsigned_Long (All_POAs (Poa.Index).Date));
      if Poa.Lifespan_Policy = PortableServer.TRANSIENT then
         Marshall (Buffer, CORBA.Unsigned_Long'(0));
      else
         Marshall_Object_Key (Buffer, Poa, 0);
      end if;
   end Marshall_Object_Key;

   procedure Unmarshall_Object_Key
     (Buffer : in out Buffer_Descriptor;
      Poa : out Broca.Poa.POA_Object_Access;
      Poa_State : out Broca.Poa.Processing_State_Type;
      Key : in out Buffer_Descriptor)
   is
      use Broca.Marshalling;
      use Broca.Poa;
      Index : Broca.Poa.Poa_Index_Type;
      Pos : Buffer_Index_Type;
      A_Long : CORBA.Unsigned_Long;
      Length : Buffer_Index_Type;
      Key_Length : Buffer_Index_Type;
      Current_Poa : Broca.Poa.POA_Object_Access;
      Old_Poa : Broca.Poa.POA_Object_Access;
      Name : CORBA.String;
      Tmp_Poa_State : Broca.Poa.Processing_State_Type;
   begin
      --  Length of the sequence.
      Unmarshall (Buffer, A_Long);
      Length := Buffer_Index_Type (A_Long);
      Pos := Buffer.Pos;

      --  Unmarshall boot time.
      Unmarshall (Buffer, A_Long);
      if A_Long /= 0 and then A_Long /= Broca.Flags.Boot_Time then
         Broca.Exceptions.Raise_Object_Not_Exist;
      end if;

      --  Unmarshall POA index.
      Unmarshall (Buffer, A_Long);
      Index := Broca.Poa.Poa_Index_Type (Natural (A_Long));

      --  Unmarshall index date.
      Unmarshall (Buffer, A_Long);

      --  Lock the table, so that we are sure the poa won't be destroyed.
      Broca.Poa.All_POAs_Lock.Lock_R;

      if All_POAs /= null
        and then Index in All_POAs.all'Range
        and then All_POAs (Index).Date = Integer (A_Long)
      then
         Poa := All_POAs (Index).Poa;

         --  Neither the POA won't be destroyed, nor its children.
         Poa.Link_Lock.Lock_R;
         Broca.Poa.All_POAs_Lock.Unlock_R;

         --  Just skip the path name.
         --  Unmarshall number of POAs in path name.
         Unmarshall (Buffer, A_Long);

         for I in 1 .. A_Long loop
            Unmarshall_Skip_String (Buffer);
         end loop;
         Inc_Usage_If_Active (Get_The_POAManager (Poa).all, Poa_State);
      else
         --  Not up to date.
         --  Unmarshall number of POAs in path name.
         Unmarshall (Buffer, A_Long);
         if A_Long = 0 then
            --  Its was an objectId for a transient POA.
            Poa := null;
            Buffer.Pos := Pos + Length;
            Broca.Poa.All_POAs_Lock.Unlock_R;
            return;
         end if;

         Current_Poa := All_POAs (Broca.Poa.Root_Poa_Index).Poa;
         --  Neither the POA won't be destroyed, nor its children.
         Current_Poa.Link_Lock.Lock_R;
         Broca.Poa.All_POAs_Lock.Unlock_R;
         while A_Long > 0 loop
            Inc_Usage_If_Active (Get_The_POAManager (Current_Poa).all,
                                 Tmp_Poa_State);
            if Tmp_Poa_State /= Active then
               Poa := Current_Poa;
               Poa_State := Tmp_Poa_State;
               Key.Pos := 0;
               return;
            end if;
            Dec_Usage (Get_The_POAManager (Current_Poa).all);
            Unmarshall (Buffer, Name);
            Old_Poa := Current_Poa;
            Current_Poa := Broca.Poa.Find_POA (Current_Poa, Name, True);
            if Current_Poa = null then
               Old_Poa.Link_Lock.Unlock_R;
               Broca.Exceptions.Raise_Object_Not_Exist;
               --  Dummy return, because never reached.
               return;
            end if;
            Current_Poa.Link_Lock.Lock_R;
            Old_Poa.Link_Lock.Unlock_R;
         end loop;
         Poa := Current_Poa;
         Inc_Usage_If_Active (Get_The_POAManager (Current_Poa).all, Poa_State);
         --  FIXME:
         --  should set the new index, the new date and raise location
         --  forward.
      end if;

      --  Length of the key
      Key_Length := Length - (Buffer.Pos - Pos);
      Increase_Buffer_And_Clear_Pos (Key, Key_Length);
      Unmarshall_Extract (Key, Buffer, Key_Length);
      return;
   end Unmarshall_Object_Key;

   --------------------------------------------------------------------------

   procedure Handle_Request (Stream : Broca.Stream.Stream_Acc;
                             Buffer : in out Buffer_Descriptor);

   --  Internal type for server_table.
   --  It defines the server for an identifier.
   type Cell is
      record
         Server : Server_Acc := null;
         --  Number of requests that can be delivered by the server.
         Count : Natural := 0;
      end record;
   type Cell_Array is array (Server_Id_Type range <>) of Cell;

   protected Server_Table is
      --  Add (register) a server in the table.
      procedure Register (Server : Server_Acc; Id : out Server_Id_Type);

      --  Called by server ID to inform it has a new request (or work)
      --  to be processed.
      procedure New_Request (Id : Server_Id_Type);

      --  Get a server having a work to be performed.
      entry Get_Server (Server : out Server_Acc);

      function Get_Server_By_Id (Id : Server_Id_Type) return Server_Acc;
   private
      Cells : Cell_Array (0 .. 3);
      --  NBR_SERVERS is also the id for the next server to be registered.
      Nbr_Servers : Server_Id_Type := 0;
      Total_Count : Natural := 0;
   end Server_Table;

   package Queues is
      --  This subpackage defines two queues for requests: the wait_queue
      --  and the hold_queue.
      --
      --  A queue contains requests, defined as:
      --    a buffer (CORBA.types.buffer_descriptor),
      --    and a connection (CORBA.stream).
      --  The buffer must be ready to be processed by handle_request, defined
      --  below.
      --
      --  It also defines and registers a server for the wait_queue.
      --
      --  POA access stored can't be dangling pointers, since it is never
      --  returned or deferenced.
      --  It is just used by unqueu_by_poa.
      type Request_Cell_Type is private;
      type Request_Cell_Acc is access Request_Cell_Type;

      protected Wait_Queue is
         --  The Wait_queue contains all requests that can be processed now.
         --  Generally, they come from the wait_queue, after the condition
         --  has been released.
         --  This queue is FIFO.

         --  Append (put) a request to the queue.
         procedure Append (Stream : Broca.Stream.Stream_Acc;
                           Buffer : Buffer_Descriptor;
                           Poa : Broca.Poa.POA_Object_Access);

         --  Form used by HOLD_QUEUE.
         --  Note: cell.next must be null.
         procedure Append (Cell : Request_Cell_Acc);

         --  Fetch the first request, and remove it from the queue.
         entry Fetch (Stream : out Broca.Stream.Stream_Acc;
                      Buffer : in out Buffer_Descriptor;
                      Poa : out Broca.Poa.POA_Object_Access);

         --  Internal use only.
--          procedure Try_Fetch (Stream: out Broca.Stream.Stream_Acc;
--                               Buffer: in out Buffer_Descriptor;
--                               Poa : out Broca.Poa.POA_Object_Access;
--                               Success: out Boolean);
      private
         --  The queue is a single linked list, with an head and a tail.
         Head : Request_Cell_Acc := null;
         Tail : Request_Cell_Acc := null;
      end Wait_Queue;

      protected Hold_Queue is
         --  The hold_queue contains all requests that can't be processes now,
         --  because:
         --    the POA is in holding state,
         --    the POA has the SINGLE_THREAD_MODEL and is processing a request.
         --    FIXME: other cases ?
         --    the POA is being created by an adapter activator.
         --
         --  A request can be removed because:
         --    the POA is in active state,
         --    the POA can proces a request,
         --    the connection was closed (not yet done).
         --
         --  The queue is FIFO, but in fact, it acts as if there were a queue
         --  for each POA.

         --  Append a request.
         procedure Append (Stream : Broca.Stream.Stream_Acc;
                           Buffer : Buffer_Descriptor;
                           Poa : Broca.Poa.POA_Object_Access);

         --  Move all requests that use poa POA to the wait_queue.
         procedure Unqueue_By_Poa
           (Poa : Broca.Poa.POA_Object_Access);
      private
         --  The queue is a single linked list, with an head and a tail.
         Head : Request_Cell_Acc := null;
         Tail : Request_Cell_Acc := null;
      end Hold_Queue;

   private
      --  Element of a queue.
      type Request_Cell_Type is
         record
            Poa : Broca.Poa.POA_Object_Access;
            Stream : Broca.Stream.Stream_Acc;
            Bd : Broca.Types.Buffer_Descriptor;
            Next : Request_Cell_Acc;
         end record;
   end Queues;

   protected body Server_Table is
      procedure Register (Server : Server_Acc; Id : out Server_Id_Type) is
      begin
         Cells (Nbr_Servers) := (Server, 0);
         Id := Nbr_Servers;
         Nbr_Servers := Nbr_Servers + 1;
      end Register;

      procedure New_Request (Id : Server_Id_Type) is
      begin
         Cells (Id).Count := Cells (Id).Count + 1;
         Total_Count := Total_Count + 1;
      end New_Request;

      entry Get_Server (Server : out Server_Acc)
      when Total_Count > 0 is
      begin
         --  Avoid wait queue getting all the servers.
         --  Serves the wait_queue at the end.
         for I in 0 .. Nbr_Servers - 1 loop
            if Cells (I).Count > 0 then
               Server := Cells (I).Server;
               Cells (I).Count := Cells (I).Count - 1;
               Total_Count := Total_Count - 1;
               return;
            end if;
         end loop;
         raise Program_Error;
      end Get_Server;

      function Get_Server_By_Id (Id : Server_Id_Type) return Server_Acc is
      begin
         if Id not in Cells'Range then
            return null;
         else
            return Cells (Id).Server;
         end if;
      end Get_Server_By_Id;
   end Server_Table;

   package body Queues is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Object => Request_Cell_Type, Name => Request_Cell_Acc);

      Wait_Server_Id : Server_Id_Type;

      protected body Wait_Queue is
         function Is_Empty return Boolean is
         begin
            return Head = null;
         end Is_Empty;

         procedure Append (Cell : Request_Cell_Acc) is
         begin
            if Cell.Next /= null then
               --  internal consistency
               Broca.Exceptions.Raise_Internal (1, Completed_No);
            end if;
            if Head = null then
               if Tail /= null then
                  --  internal consistency.
                  Broca.Exceptions.Raise_Internal (2, Completed_No);
               end if;
               Head := Cell;
               Tail := Cell;
            else
               Tail.Next := Cell;
               Tail := Cell;
            end if;
            Server_Table.New_Request (Wait_Server_Id);
         end Append;

         procedure Append (Stream : Broca.Stream.Stream_Acc;
                           Buffer : Buffer_Descriptor;
                           Poa : Broca.Poa.POA_Object_Access) is
         begin
            --  Simply encapsulate the arguments into a cell.
            Append (new Request_Cell_Type'
                    (Poa => Poa,
                     Stream => Stream,
                     Bd => Buffer,
                     Next => null));
         end Append;

         procedure Try_Fetch (Stream : out Broca.Stream.Stream_Acc;
                              Buffer : in out Buffer_Descriptor;
                              Poa : out Broca.Poa.POA_Object_Access;
                              Success : out Boolean)
         is
            Cell : Request_Cell_Acc;
         begin
            if Head = null then
               Success := False;
               return;
            else
               Success := True;
            end if;

            Cell := Head;
            if Cell = Tail then
               --  There was only one cell, make the queue empty.
               Tail := null;
               Head := null;
            else
               Head := Head.Next;
            end if;
            --  Free the memory associed with BUFFER, since it is overwritten.
            Unchecked_Deallocation (Buffer.Buffer);
            Stream := Cell.Stream;
            Buffer := Cell.Bd;
            Poa := Cell.Poa;
            Unchecked_Deallocation (Cell);
         end Try_Fetch;

         entry Fetch (Stream : out Broca.Stream.Stream_Acc;
                      Buffer : in out Buffer_Descriptor;
                      Poa : out Broca.Poa.POA_Object_Access)
         when Head /= null is
            Res : Boolean;
         begin
            Try_Fetch (Stream, Buffer, Poa, Res);
            if not Res then
               raise Program_Error;
            end if;
         end Fetch;
      end Wait_Queue;

      --  Define a pseudo-server for the wait_queue.
      --  Note: there is no profiles for this server.
      type Wait_Server_Type is new Server_Type with null record;
      procedure Perform_Work (Server : access Wait_Server_Type;
                              Buffer : in out Broca.Types.Buffer_Descriptor);
      procedure Marshall_Size_Profile
        (Server : access Wait_Server_Type;
         Ior : in out Broca.Types.Buffer_Descriptor;
         Object_Key : Broca.Types.Buffer_Descriptor);
      procedure Marshall_Profile (Server : access Wait_Server_Type;
                                  Ior : in out Broca.Types.Buffer_Descriptor;
                                  Object_Key : Broca.Types.Buffer_Descriptor);

      procedure Perform_Work (Server : access Wait_Server_Type;
                              Buffer : in out Broca.Types.Buffer_Descriptor)
      is
         use Broca.Poa;
         Stream : Broca.Stream.Stream_Acc;
         Poa : Broca.Poa.POA_Object_Access;
      begin
         --  Simply get an entry...
         Queues.Wait_Queue.Fetch (Stream, Buffer, Poa);
         --  ... and handles (processes) it.
         if Poa /= null then
            Broca.Poa.Cleanup (Poa);
         else
            Handle_Request (Stream, Buffer);
         end if;
      end Perform_Work;

      procedure Marshall_Size_Profile
        (Server : access Wait_Server_Type;
         Ior : in out Broca.Types.Buffer_Descriptor;
         Object_Key : Broca.Types.Buffer_Descriptor) is
      begin
         return;
      end Marshall_Size_Profile;

      procedure Marshall_Profile (Server : access Wait_Server_Type;
                                  Ior : in out Broca.Types.Buffer_Descriptor;
                                  Object_Key : Broca.Types.Buffer_Descriptor)
      is
      begin
         return;
      end Marshall_Profile;

      protected body Hold_Queue is
         procedure Append (Stream : Broca.Stream.Stream_Acc;
                           Buffer : Buffer_Descriptor;
                           Poa : Broca.Poa.POA_Object_Access)
         is
            Cell : Request_Cell_Acc;
         begin
            Cell := new Request_Cell_Type'
              (Poa => Poa,
               Stream => Stream,
               Bd => Buffer,
               Next => null);
            if Head = null then
               if Tail /= null then
                  --  internal consistency.
                  Broca.Exceptions.Raise_Internal (3, Completed_No);
               end if;
               Head := Cell;
               Tail := Cell;
            else
               Tail.Next := Cell;
               Tail := Cell;
            end if;
         end Append;

         procedure Unqueue_By_Poa (Poa : Broca.Poa.POA_Object_Access)
         is
            use Broca.Poa;
            Cell, Prev_Cell : Request_Cell_Acc;
         begin
            --  Unqueue messages.

            --  The head.
            while Head /= null and then Head.Poa = Poa loop
               Cell := Head;
               Head := Cell.Next;
               if Tail = Cell then
                  if Head /= null then
                     --  Consistency check.
                     Broca.Exceptions.Raise_Internal (4, Completed_No);
                  end if;
                  Tail := null;
               end if;
               Cell.Next := null;
               Wait_Queue.Append (Cell);
            end loop;

            --  The next.
            if Head = null then
               return;
            end if;
            Prev_Cell := Head;
            while Prev_Cell.Next /= null loop
               while Prev_Cell.Next.Poa = Poa loop
                  Cell := Prev_Cell.Next;
                  Prev_Cell.Next := Cell.Next;
                  if Tail = Cell then
                     if Cell.Next /= null then
                        --  Consistency check.
                        Broca.Exceptions.Raise_Internal (5, Completed_No);
                     end if;
                     Tail := null;
                  end if;
                  Cell.Next := null;
                  Wait_Queue.Append (Cell);
               end loop;
            end loop;
         end Unqueue_By_Poa;
      end Hold_Queue;
   begin
      --  Create and register the server for the wait_queue.
      Server_Table.Register (new Wait_Server_Type, Wait_Server_Id);
   end Queues;


   --  Process a GIOP request.
   --  the current position of buffer must be a GIOP RequestHeader.
   procedure Handle_Request (Stream : Broca.Stream.Stream_Acc;
                             Buffer : in out Buffer_Descriptor) is
      use Broca.Poa;
      use Broca.Marshalling;
      use Broca.Stream;
      A_Long : CORBA.Unsigned_Long;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Operation : CORBA.String;
      Principal : CORBA.String;
      Poa : Broca.Poa.POA_Object_Access;
      Poa_State : Broca.Poa.Processing_State_Type;
      Pos : Buffer_Index_Type;
      Key : Buffer_Descriptor;
   begin
      Pos := Buffer.Pos;
      --  service_context
      Unmarshall (Buffer, A_Long);
      if A_Long /= 0 then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      --  request id
      Unmarshall (Buffer, Request_Id);

      --  reponse expected
      Unmarshall (Buffer, Reponse_Expected);

      --  Object key
      Unmarshall_Object_Key (Buffer, Poa, Poa_State, Key);

      case Poa_State is
         when Active =>
            Log ("invoke method");

            --  Operation
            Unmarshall (Buffer, Operation);

            --  principal
            Unmarshall (Buffer, Principal);

            begin
               --  This unlock_R the POA.
               Broca.Poa.Giop_Invoke
                 (Poa, Key, CORBA.Identifier (Operation),
                  Request_Id, Reponse_Expected, Buffer);
            exception
               when E : CORBA.Object_Not_Exist =>
                  Broca.Giop.Create_Reply_System_Exception
                    (Buffer, Request_Id, E);
               when E : PortableServer.ForwardRequest =>
                  declare
                     Fr_M : PortableServer.ForwardRequest_Members;
                  begin
                     PortableServer.Get_Members (E, Fr_M);
                     Broca.Giop.Create_Reply_Location_Forward
                       (Buffer, Request_Id, Fr_M.Forward_Reference);
                  end;
            end;

            Lock_Send (Stream);
            Send (Stream, Buffer);
            Unlock_Send (Stream);

         when Discarding =>
            Poa.Link_Lock.Unlock_R;
            Log ("discard request");

            --  Not very efficient!
            begin
               Broca.Exceptions.Raise_Transient;
            exception
               when E : CORBA.Transient =>
                  Broca.Giop.Create_Reply_System_Exception
                    (Buffer, Request_Id, E);
            end;
            Lock_Send (Stream);
            Send (Stream, Buffer);
            Unlock_Send (Stream);

         when Holding =>
            Poa.Link_Lock.Unlock_R;
            Log ("queue request");

            --  Queue this request
            Buffer.Pos := Pos;
            Queues.Hold_Queue.Append (Stream, Buffer, Poa);
            Buffer.Buffer := null;

         when Inactive =>
            Poa.Link_Lock.Unlock_R;
            Log ("rejected request");

            --  Not very efficient!
            begin
               Broca.Exceptions.Raise_Obj_Adapter;
            exception
               when E : CORBA.Obj_Adapter =>
                  Broca.Giop.Create_Reply_System_Exception
                    (Buffer, Request_Id, E);
            end;
            Lock_Send (Stream);
            Send (Stream, Buffer);
            Unlock_Send (Stream);

      end case;
      Unchecked_Deallocation (Key.Buffer);
   exception
      when Broca.Stream.Connection_Closed =>
         null;
   end Handle_Request;

   --  Handle A GIOP message coming from stream STREAM.
   --  BUFFER must contain an unprocessed message header.
   procedure Handle_Message (Stream : Broca.Stream.Stream_Acc;
                             Buffer : in out Buffer_Descriptor) is
      use Broca.Poa;
      use Broca.Marshalling;
      use Broca.Stream;
      Message_Type : CORBA.Octet;
      Message_Size : CORBA.Unsigned_Long;
      Request_Id : CORBA.Unsigned_Long;
      Poa : Broca.Poa.POA_Object_Access;
      Poa_State : Broca.Poa.Processing_State_Type;
      Key : Buffer_Descriptor;
   begin
      --  Magic must be GIOP.
      if Buffer.Buffer (0 .. 3) /= Broca.Giop.Magic then
         --  Must send an error message.
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Check version
      if Buffer.Buffer (4) /= 1 or else Buffer.Buffer (5) /= 0 then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  byte_order
      Buffer.Pos := 6;
      Unmarshall (Buffer, Buffer.Little_Endian);

      --  Message type and length.
      Unmarshall (Buffer, Message_Type);
      Unmarshall (Buffer, Message_Size);

      --  Receive body of the message.
      Increase_Buffer_And_Set_Pos
        (Buffer, Buffer_Index_Type (Message_Size));
      Receive (Stream, Buffer);
      Unlock_Receive (Stream);
      Buffer.Pos := 0;

      case CORBA.Unsigned_Long (Message_Type) is
         when Broca.Giop.Request =>
            Log ("handle request message");

            Handle_Request (Stream, Buffer);

         when Broca.Giop.Locate_Request =>
            Log ("handle locate_request message");

            --  request id
            Unmarshall (Buffer, Request_Id);

            --  Object key
            Unmarshall_Object_Key (Buffer, Poa, Poa_State, Key);
            Poa.Link_Lock.Unlock_R;

            --  FIXME.
            Broca.Giop.Create_Giop_Header
              (Buffer, Broca.Giop.Locate_Reply, 8);
            Marshall (Buffer, Request_Id);
            Marshall (Buffer, Broca.Giop.Object_Here);
            Lock_Send (Stream);
            Send (Stream, Buffer);
            Unlock_Send (Stream);
            Unchecked_Deallocation (Key.Buffer);

         when others =>
            Broca.Exceptions.Raise_Comm_Failure;
      end case;
   exception
      when Broca.Stream.Connection_Closed =>
         null;
   end Handle_Message;

   procedure New_Request (Id : Server_Id_Type) is
   begin
      Server_Table.New_Request (Id);
   end New_Request;

   procedure Register (Server : Server_Acc; Id : out Server_Id_Type) is
   begin
      Server_Table.Register (Server, Id);
   end Register;

   procedure Build_Ior (Target : out Broca.Types.Buffer_Descriptor;
                        Type_Id : CORBA.RepositoryId;
                        Poa : Broca.Poa.POA_Object_Access;
                        Key : Broca.Types.Buffer_Descriptor) is
      use Broca.Marshalling;
      Ior : Buffer_Descriptor;
      Object_Key : Broca.Types.Buffer_Descriptor;
      Pos : Buffer_Index_Type;
      Nbr_Profiles : CORBA.Unsigned_Long;
      Server : Server_Acc;
      Length : CORBA.Unsigned_Long;
   begin
      --  Lock the POA.  As a result, we are sure it won't be destroyed
      --  during the marshalling of the IOR.
      --  FIXME: catch exceptions.
      Poa.Link_Lock.Lock_R;

      --  Create Object_Key.
      Marshall_Size_Unsigned_Long (Object_Key);
      Marshall_Size_Object_Key (Object_Key, Poa);
      Marshall_Size_Append (Object_Key, Key);
      Length := CORBA.Unsigned_Long (Object_Key.Pos - 4);
      Allocate_Buffer (Object_Key);
      Marshall (Object_Key, Length);
      Marshall_Object_Key (Object_Key, Poa);
      Marshall_Append (Object_Key, Key);
      Poa.Link_Lock.Unlock_R;

      Ior.Pos := 0;
      --  An ior is an encapsulation.
      Marshall_Size_Octet (Ior);
      --  The type_id
      Marshall_Size (Ior, CORBA.String (Type_Id));
      --  nbr of profiles
      Marshall_Size_Unsigned_Long (Ior);

      Nbr_Profiles := 0;
      for N in Server_Id_Type loop
         Server := Server_Table.Get_Server_By_Id (N);
         exit when Server = null;
         Pos := Ior.Pos;
         Marshall_Size_Profile (Server, Ior, Object_Key);
         if Ior.Pos /= Pos then
            Nbr_Profiles := Nbr_Profiles + 1;
         end if;
      end loop;

      if Nbr_Profiles = 0 then
         Ior.Pos := 0;
         Target := Ior;
         return;
      end if;

      Allocate_Buffer (Ior);

      Marshall (Ior, Is_Little_Endian);
      Marshall (Ior, CORBA.String (Type_Id));
      Marshall (Ior, Nbr_Profiles);
      for N in Server_Id_Type loop
         Server := Server_Table.Get_Server_By_Id (N);
         exit when Server = null;
         Marshall_Profile (Server, Ior, Object_Key);
      end loop;

      Unchecked_Deallocation (Object_Key.Buffer);

      Target := Ior;
   end Build_Ior;

   --  Create an ORB server.
   type This_Orb_Type is new Broca.Orb.Orb_Type with null record;
   procedure Run (Orb : in out This_Orb_Type);
   procedure Poa_State_Changed
     (Orb : in out This_Orb_Type; Poa : Broca.Poa.POA_Object_Access);

   procedure Serv;

   procedure Serv is
      Buffer : Buffer_Descriptor;
      Server : Server_Acc;
   begin
      loop
         Server_Table.Get_Server (Server);
         Perform_Work (Server, Buffer);
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put ("Orb task Server: exception ");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put (": ");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line ("Orb task Server shut down");
   end Serv;

   procedure Run (Orb : in out This_Orb_Type) is
      task type Server_Task_Type is
      end Server_Task_Type;

      task body Server_Task_Type is
      begin
         Serv;
      end Server_Task_Type;

      type Server_Task_Array is array (Natural range <>) of Server_Task_Type;
      Server_Taks : Server_Task_Array (1 .. Broca.Flags.Nbr_Server_Tasks - 1);
      pragma Warnings (Off, Server_Taks);
   begin
      Serv;
   end Run;

   procedure Poa_State_Changed
     (Orb : in out This_Orb_Type; Poa : Broca.Poa.POA_Object_Access)
   is
   begin
      Log ("unqueue requests");
      Queues.Hold_Queue.Unqueue_By_Poa (Poa);
   end Poa_State_Changed;

   --  This procedure is called by a POA to request a server task to perform
   --  arbitrary work, such as cleaning the POA up.
   procedure Request_Cleanup (Poa : Broca.Poa.POA_Object_Access)
   is
      Bd : Broca.Types.Buffer_Descriptor;
   begin
      Queues.Wait_Queue.Append (null, Bd, Poa);
   end Request_Cleanup;

begin
   Broca.Orb.Register_Orb
     (new This_Orb_Type'(Broca.Orb.Orb_Type with null record));
end Broca.Server;
