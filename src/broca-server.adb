------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . S E R V E R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Task_Identification;

with CORBA; use CORBA;
with CORBA.Impl;
with PortableServer;

with Broca.Buffers;     use Broca.Buffers;
with Broca.CDR;         use Broca.CDR;
with Broca.Opaque;
with Broca.Exceptions;
with Broca.GIOP;
with Broca.Parameters;
with Broca.Stream;
with Broca.Flags;
with Broca.ORB;
with Broca.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Server is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.server");
   procedure O is new Broca.Debug.Output (Flag);

   procedure Marshall
     (Buffer : access Buffer_Type;
      POA    : in Broca.POA.Ref;
      Num    : in Natural);
   --  Marshall POA chain.

   procedure Log (S : String);

   --  Display a better log message
   procedure Log (S : String) is
   begin
      pragma Debug (O ("[" & Ada.Task_Identification.Image
                       (Ada.Task_Identification.Current_Task)
                       & "] ORB log: " & S));
      null;
   end Log;

   use Broca.POA;

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
   --
   --  FIXME: I am not sure that this is the right thing to do. I wonder
   --  what will happen if a client has a lifetime which spans over
   --  a persistent POA stop/restart. If a location forward has been sent,
   --  then this client will not be able to use services of the POA anymore
   --  because the new location does not exist anymore. If we do not use
   --  location forward, then this case is handled gracefully but
   --  performances may be slightly degraded.

   type POA_Entry_Type is
      record
         POA : Broca.POA.Ref;
         --  The date is incremented during unregister.
         --  FIXME: Overflow is not handled
         Date : Natural := 0;
      end record;

   type POA_Entry_Array is array (Broca.POA.POA_Index_Type range <>)
     of POA_Entry_Type;
   type POA_Entry_Array_Ptr is access POA_Entry_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => POA_Entry_Array, Name => POA_Entry_Array_Ptr);

   --  This is the variable containing an access to the table of objects.
   All_POAs : POA_Entry_Array_Ptr := null;

   --  Find a free entry in the table of objects or expand it.
   procedure Register_POA (POA : Broca.POA.Ref)
   is
      The_POA : constant POA_Object_Ptr
        := POA_Object_Of (POA);
   begin
      Log ("Register_Poa: "
           & CORBA.To_Standard_String (The_POA.Name));

      if All_POAs = null then
         --  The table was never built.
         All_POAs := new POA_Entry_Array (1 .. 16);
         All_POAs (1) := (POA => POA, Date => 1);
         The_POA.Index := 1;
      else
         --  Try to find a free entry.
         --  FIXME: optimize here.
         for I in All_POAs.all'Range loop
            if Is_Nil (All_POAs (I).POA) then
               --  An entry was found.
               All_POAs (I).POA := POA;
               The_POA.Index := I;
               return;
            end if;
         end loop;

         --  Expand the table.
         declare
            N_Ao : POA_Entry_Array_Ptr;
            O_Ao : POA_Entry_Array_Ptr;
            Slot : POA_Index_Type;
         begin
            N_Ao := new POA_Entry_Array (1 .. 2 * All_POAs.all'Last);
            N_Ao (All_POAs.all'Range) := All_POAs.all;
            Slot := All_POAs.all'Last + 1;
            N_Ao (Slot) := (POA => POA, Date => 1);
            O_Ao := All_POAs;
            All_POAs := N_Ao;
            Free (O_Ao);
            The_POA.Index := Slot;
         end;
      end if;
   end Register_POA;

   procedure Unregister_POA (POA : Broca.POA.Ref)
   is
      Index : constant POA_Index_Type
        := POA_Object_Of (POA).Index;
   begin
      All_POAs (Index)
        := (POA => Nil_Ref, Date => All_POAs (Index).Date + 1);
   end Unregister_POA;

   --  Create an object_key.
   procedure Handle_Request
     (Stream : Broca.Stream.Stream_Ptr;
      Buffer : access Buffer_Type;
      Asynchronous : Boolean := False);

   --  Internal type for Server_Table.
   --  It defines the server for an identifier.
   type Cell is
      record
         Server : Server_Ptr := null;
         --  Number of requests that can be delivered by the server.
         Count : Natural := 0;
      end record;
   type Cell_Array is array (Server_Id_Type range <>) of Cell;

   protected Server_Table is
      --  Add (register) a server in the table.
      procedure Register (Server : Server_Ptr; Id : out Server_Id_Type);

      --  Called by server ID to inform it has a new request (or work)
      --  to be processed.
      procedure New_Request (Id : Server_Id_Type);

      --  Get a server having a work to be performed.
      entry Get_Server (Server : out Server_Ptr);

      function Get_Server_By_Id (Id : Server_Id_Type) return Server_Ptr;
   private
      Cells : Cell_Array (0 .. 3);
      --  Nbr_Servers is also the id for the next server
      --  to be registered.
      Nbr_Servers : Server_Id_Type := 0;
      Total_Count : Natural := 0;
   end Server_Table;

   package Queues is
      --  This subpackage defines two queues for requests:
      --  the Wait_Queue and the Hold_Queue.
      --
      --  A queue contains requests, defined as:
      --    a buffer (Broca.Buffers.Buffer_Type)
      --    and a connection (Broca.Stream.Stream_Ptr).
      --  The buffer must be ready to be processed by
      --  Handle_Request, defined below.
      --
      --  It also defines and registers a server for the
      --  Wait_Queue.
      --
      --  POA access stored can't be dangling pointers, since
      --  it is never returned or deferenced.
      --  It is only used by Unqueue_By_POA.

      type Request_Cell_Type is private;
      type Request_Cell_Ptr is access Request_Cell_Type;

      protected Wait_Queue is
         --  The Wait_Queue contains all requests that can be
         --  processed now. Generally, they come from the
         --  Wait_Queue, after the condition has been released.
         --  This queue is FIFO.

         --  Append (put) a request to the queue.
         procedure Append (Stream : Broca.Stream.Stream_Ptr;
                           Buffer : Buffer_Access;
                           POA : Broca.POA.Ref);

         --  Prepend a request in front of the queue
         procedure Prepend
           (Stream : Broca.Stream.Stream_Ptr;
            Buffer : Buffer_Access);

         --  Form used by HOLD_QUEUE.
         --  Note: Cell.Next must be null.
         procedure Append (Cell : Request_Cell_Ptr);

         --  Fetch the first request, and remove it from the queue.
         entry Fetch (Stream : out Broca.Stream.Stream_Ptr;
                      Buffer : out Buffer_Access;
                      POA : out Broca.POA.Ref);

      private
         --  The queue is a single linked list, with an head and a tail.
         Head : Request_Cell_Ptr := null;
         Tail : Request_Cell_Ptr := null;
      end Wait_Queue;

      protected Hold_Queue is
         --  The Hold_Queue contains all requests that can't
         --  be processed now because:
         --   - the POA is in holding state,
         --   - the POA has the SINGLE_THREAD_MODEL and is
         --     busy processing a request.
         --   - the POA is being created by an adapter activator.
         --    FIXME: other cases ?
         --
         --  A request can be removed because:
         --   - the POA is in active state,
         --   - the POA can proces a request,
         --   - the connection was closed (not yet done).
         --
         --  The queue is FIFO, but in fact, it acts as if there were a queue
         --  for each POA.

         --  Append a request. Make a copy of buffer.
         procedure Append (Stream : Broca.Stream.Stream_Ptr;
                           Buffer : Buffer_Access;
                           POA    : Broca.POA.Ref);

         --  Move all requests that use POA to the Wait_Queue.
         procedure Unqueue_By_POA
           (POA : Broca.POA.Ref);
      private
         --  The queue is a single linked list, with an head and a tail.
         Head : Request_Cell_Ptr := null;
         Tail : Request_Cell_Ptr := null;
      end Hold_Queue;

   private
      --  Element of a queue.
      type Request_Cell_Type is
         record
            POA    : Broca.POA.Ref;
            Stream : Broca.Stream.Stream_Ptr;
            Bd     : Buffer_Access;
            Next   : Request_Cell_Ptr;
         end record;
   end Queues;

   protected body Server_Table is
      procedure Register (Server : Server_Ptr; Id : out Server_Id_Type) is
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

      entry Get_Server (Server : out Server_Ptr)
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

      function Get_Server_By_Id (Id : Server_Id_Type) return Server_Ptr is
      begin
         if Id not in Cells'Range then
            return null;
         else
            return Cells (Id).Server;
         end if;
      end Get_Server_By_Id;
   end Server_Table;

   package body Queues is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Request_Cell_Type, Name => Request_Cell_Ptr);

      Wait_Server_Id : Server_Id_Type;

      protected body Wait_Queue is

         procedure Append (Stream : Broca.Stream.Stream_Ptr;
                           Buffer : Buffer_Access;
                           POA : Broca.POA.Ref) is
         begin
            Append (new Request_Cell_Type'(POA    => POA,
                                           Stream => Stream,
                                           Bd     => Buffer,
                                           Next   => null));
         end Append;

         procedure Append (Cell : Request_Cell_Ptr) is
         begin
            pragma Assert (not (Head = null xor Tail = null));
            pragma Assert (Cell.Next = null);
            if Tail = null then
               Head := Cell;
               Tail := Cell;
            else
               Tail.Next := Cell;
               Tail := Cell;
            end if;
            Server_Table.New_Request (Wait_Server_Id);
         end Append;

         procedure Prepend
           (Stream : Broca.Stream.Stream_Ptr;
            Buffer : Buffer_Access) is
         begin
            pragma Assert (not (Head = null xor Tail = null));
            Head := new Request_Cell_Type'(POA    => Nil_Ref,
                                           Stream => Stream,
                                           BD     => Buffer,
                                           Next   => Head);
            if Tail = null then
               Tail := Head;
            end if;
            Server_Table.New_Request (Wait_Server_Id);
         end Prepend;

         entry Fetch
           (Stream : out Broca.Stream.Stream_Ptr;
            Buffer : out Buffer_Access;
            POA    : out Broca.POA.Ref)
         when Head /= null
         is
            Old : Request_Cell_Ptr := Head;
         begin
            pragma Assert (Tail /= null);
            Stream := Head.Stream;
            Buffer := Head.Bd;
            POA    := Head.POA;
            Free (Old);
            Head := Head.Next;
            if Head = null then
               Tail := null;
            end if;
         end Fetch;

      end Wait_Queue;

      --  Define a pseudo-server for the Wait_Queue.
      --  Note: there is no profiles for this server.
      type Wait_Server_Type is new Server_Type with null record;

      procedure Perform_Work
        (Server : access Wait_Server_Type);

      function Can_Create_Profile
        (Server : access Wait_Server_Type)
        return Boolean;

      function Make_Profile
        (Server : access Wait_Server_Type;
         Object_Key : Encapsulation)
         return Broca.IOP.Profile_Ptr;

      procedure Perform_Work
        (Server : access Wait_Server_Type)
      is
         use Broca.POA;
         Stream : Broca.Stream.Stream_Ptr;
         POA : Broca.POA.Ref;
         Buffer : Buffer_Access;
      begin
         pragma Debug (O ("Perform_Work: enter"));
         --  Simply get an entry...
         Queues.Wait_Queue.Fetch (Stream, Buffer, POA);
         pragma Debug (O ("Perform_Work: got a new dequeued job"));
         --  ... and handles (processes) it.
         if not Is_Nil (POA) then
            Broca.POA.Cleanup (POA_Object_Of (POA));
         else
            pragma Debug (O ("Perform_Work: executing an asynchronous job"));
            Handle_Request (Stream, Buffer, Asynchronous => True);
            Release (Buffer);
         end if;
      end Perform_Work;

      function Can_Create_Profile
        (Server : access Wait_Server_Type)
        return Boolean is
      begin
         return False;
         --  The Wait server does not manage connections
         --  by itself, and therefore cannot create a
         --  profile associated to an object.
      end Can_Create_Profile;

      function Make_Profile
        (Server : access Wait_Server_Type;
         Object_Key : Encapsulation)
        return Broca.IOP.Profile_Ptr is
      begin
         raise Program_Error;

         return null;
      end Make_Profile;

      protected body Hold_Queue is
         procedure Append (Stream : Broca.Stream.Stream_Ptr;
                           Buffer : Buffer_Access;
                           POA : Broca.POA.Ref)
         is
            Cell  : Request_Cell_Ptr;
         begin
            Cell := new Request_Cell_Type'
              (POA => POA,
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

         procedure Unqueue_By_POA (POA : Broca.POA.Ref)
         is
            use Broca.POA;
            Cell, Prev_Cell : Request_Cell_Ptr;
         begin
            --  Unqueue messages.

            --  The head.
            while Head /= null and then Head.POA = POA loop
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
               while Prev_Cell.Next.POA = POA loop
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
         end Unqueue_By_POA;
      end Hold_Queue;
   begin
      --  Create and register the server for the wait_queue.
      Server_Table.Register (new Wait_Server_Type, Wait_Server_Id);
   end Queues;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Stream : Broca.Stream.Stream_Ptr;
      Buffer : access Buffer_Type;
      Asynchronous : Boolean := False)
   is
      use Broca.POA;
      use Broca.Stream;

      --  Process a GIOP request. The current position of buffer must
      --  be a GIOP RequestHeader.

      Context           : CORBA.Unsigned_Long;
      Request_Id        : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Operation         : CORBA.String;
      Principal         : CORBA.String;
      POA               : Broca.POA.Ref;
      POA_State         : Broca.POA.Processing_State_Type;

      Reply_Buffer : aliased Buffer_Type;

   begin
      pragma Debug (O ("Handle_Request : enter"));

      --  FIXME: This should be encapsulated.
      --  Prepare the reply body buffer to hold
      --  the reply message data.
      Set_Initial_Position
        (Reply_Buffer'Access,
         GIOP.Message_Header_Size);

      --  Service context
      Context := Unmarshall (Buffer);
      if Context /= Broca.GIOP.No_Context then
         pragma Debug (O ("Handle_Request: Incorrect context" & Context'Img));
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      --  Request Id
      Request_Id := Unmarshall (Buffer);
      pragma Debug (O ("Handle_Request: Request_Id" & Request_Id'Img));

      --  Response expected
      pragma Debug (O ("Handle_Request: unmarshalling Response_Expected"));
      Response_Expected := Unmarshall (Buffer);

      declare
         Key : aliased Encapsulation := Unmarshall (Buffer);
         Key_Buffer : aliased Buffer_Type;

      begin
         Decapsulate (Key'Access, Key_Buffer'Access);
         pragma Debug (O ("Handle_Request : unmarshalling POA"));
         Unmarshall (Key_Buffer'Access, POA, POA_State);

         case POA_State is
            when Active =>

               pragma Debug (O ("Handle_Request : POA is active"));
               Log ("invoke method");

               --  If no response is expected and the current context is
               --  synchronous, requeue the request for an asynchronous
               --  handling. Then just leave, our work is over.

               if not Response_Expected and then not Asynchronous then
                  pragma Debug
                    (O ("Handle_Request: requeuing asynchronous request"));
                  POA_Object_Of (POA).Link_Lock.Unlock_R;
                  Queues.Wait_Queue.Prepend (Stream, Copy (Buffer));
                  Release (Reply_Buffer);
                  return;
               end if;

               --  Operation
               pragma Debug (O ("Handle_Request : unmarshalling operation"));
               Operation := Unmarshall (Buffer);

               --  Principal
               pragma Debug (O ("Handle_Request : unmarshalling principal"));
               Principal := Unmarshall (Buffer);

               begin
                  --  This Unlock_R the POA.
                  pragma Debug (O ("Handle_Request : invoking "
                                   & To_Standard_String (Operation)));

                  declare
                     Object_Id : aliased Encapsulation
                       := Unmarshall (Key_Buffer'Access);
                  begin
                     Broca.POA.GIOP_Invoke
                       (POA_Object_Of (POA),
                        Object_Id'Access,
                        CORBA.Identifier (Operation),
                        Request_Id, Response_Expected,
                        Buffer,
                        Reply_Buffer'Access);

                     Broca.GIOP.Prepend_GIOP_Header
                       (Reply_Buffer'Access,
                        Broca.GIOP.Reply);
                  end;

               exception
                  when E : CORBA.Object_Not_Exist =>

                     begin
                        Broca.GIOP.Marshall
                          (Reply_Buffer'Access, Request_Id, E);

                        Broca.GIOP.Prepend_GIOP_Header
                          (Reply_Buffer'Access,
                           Broca.GIOP.Reply);
                     end;

                  when E : PortableServer.ForwardRequest =>
                     declare
                        FRM : PortableServer.ForwardRequest_Members;
                     begin
                        PortableServer.Get_Members (E, FRM);
                        Broca.GIOP.Marshall
                          (Reply_Buffer'Access,
                           Request_Id, FRM.Forward_Reference);

                        Broca.GIOP.Prepend_GIOP_Header
                          (Reply_Buffer'Access,
                           Broca.GIOP.Reply);
                     end;
               end;

               begin
                  if Response_Expected then
                     Lock_Send (Stream);
                     Send (Stream, Reply_Buffer'Access);
                     Unlock_Send (Stream);
                  else
                     pragma Debug
                       (O ("Handle_Request: not sending unexpected response"));
                     null;
                  end if;

                  Release (Reply_Buffer);
               exception
                  when Connection_Closed =>
                     --  We could not send the answer
                     pragma Debug (O ("Handle_Request: cannot send reply"));
                     Release (Reply_Buffer);
               end;

            when Discarding =>

               POA_Object_Of (POA).Link_Lock.Unlock_R;
               Log ("discard request");

               --  Not very efficient!
               declare
               begin
                  Broca.Exceptions.Raise_Transient;
               exception
                  when E : CORBA.Transient =>
                     Broca.GIOP.Marshall (Reply_Buffer'Access, Request_Id, E);

                     Broca.GIOP.Prepend_GIOP_Header
                       (Reply_Buffer'Access,
                        Broca.GIOP.Reply);
               end;
               Lock_Send (Stream);
               Send (Stream, Buffer);
               Unlock_Send (Stream);

            when Holding =>

               POA_Object_Of (POA).Link_Lock.Unlock_R;
               Log ("queue request");

               --  Queue this request
               Queues.Hold_Queue.Append (Stream, Copy (Buffer), POA);

            when Inactive =>

               POA_Object_Of (POA).Link_Lock.Unlock_R;
               Log ("rejected request");

               --  Not very efficient!
               begin
                  Broca.Exceptions.Raise_Obj_Adapter;
               exception
                  when E : CORBA.Obj_Adapter =>

                     Broca.GIOP.Marshall
                       (Reply_Buffer'Access, Request_Id, E);

                     Broca.GIOP.Prepend_GIOP_Header
                       (Reply_Buffer'Access,
                        Broca.GIOP.Reply);
               end;
               Lock_Send (Stream);
               Send (Stream, Buffer);
               Unlock_Send (Stream);

         end case;
      end;

      pragma Debug (O ("Handle_Request : leave"));
   exception
      when Broca.Stream.Connection_Closed =>
         null;
   end Handle_Request;

   --  Handle A GIOP message coming from stream STREAM.
   --  BUFFER must contain an unprocessed message header.
   procedure Handle_Message
     (Stream : in Broca.Stream.Stream_Ptr;
      Buffer : access Buffer_Type)
   is
      use Broca.GIOP;
      use Broca.Stream;

      Message_Type : MsgType;
      Message_Size : CORBA.Unsigned_Long;
      Message_Endianness : Endianness_Type;
      Request_Id : CORBA.Unsigned_Long;
      Header_Correct : Boolean;

   begin
      Unmarshall_GIOP_Header (Buffer,
                              Message_Type, Message_Size,
                              Message_Endianness,
                              Header_Correct);

      if not Header_Correct then
         --  The received GIOP message header is erroneous.
         declare
            Reply : aliased Buffer_Type;
         begin
            --  The reply is an error, and consists only in a
            --  message header.

            Broca.GIOP.Marshall_GIOP_Header
              (Buffer,
               Broca.GIOP.Message_Error,
               0);

            Lock_Send (Stream);
            Send (Stream, Reply'Access);
            Unlock_Send (Stream);

            Release (Reply);
            return;
         end;
      end if;

      --  Receive body of the message.
      declare
         Message_Body : Broca.Opaque.Octet_Array_Ptr
           := Receive (Stream,
                       Broca.Opaque.Index_Type (Message_Size));
         Message_Body_Buffer : aliased Buffer_Type;
      begin
         Broca.Buffers.Initialize_Buffer
           (Message_Body_Buffer'Access,
            Broca.Opaque.Index_Type (Message_Size),
            Message_Body.all'Address,
            Message_Endianness,
            GIOP.Message_Header_Size);

         Unlock_Receive (Stream);

         case Message_Type is
            when Broca.GIOP.Request =>
               Log ("handle request message");

               Handle_Request (Stream, Message_Body_Buffer'Access);

            when Broca.GIOP.Locate_Request =>
               Log ("handle locate_request message");

               declare
                  Reply_Buffer : aliased Buffer_Type;
               begin
                  --  FIXME: This should be encapsulated.
                  --  Prepare the reply body buffer to hold
                  --  the reply message data.
                  Set_Initial_Position
                    (Reply_Buffer'Access,
                     GIOP.Message_Header_Size);

                  --  Request Id
                  Request_Id := Unmarshall
                    (Message_Body_Buffer'Access);

                  --  ObjectKey ??

                  --  FIXME: Should add the possibility
                  --     to reply OBJECT_FORWARD
                  Marshall (Reply_Buffer'Access, Request_Id);
                  Broca.GIOP.Marshall
                    (Reply_Buffer'Access, Broca.GIOP.Object_Here);

                  Broca.GIOP.Prepend_GIOP_Header
                    (Reply_Buffer'Access, Broca.GIOP.Locate_Reply);

                  Lock_Send (Stream);
                  Send (Stream, Reply_Buffer'Access);
                  Unlock_Send (Stream);
                  Release (Reply_Buffer);
               end;
            when others =>
               Release (Message_Body_Buffer);
               Broca.Opaque.Free (Message_Body);
               Broca.Exceptions.Raise_Comm_Failure;
         end case;

         Release (Message_Body_Buffer);
         Broca.Opaque.Free (Message_Body);
      end;
   exception
      when Broca.Stream.Connection_Closed =>
         null;
   end Handle_Message;

   procedure New_Request (Id : Server_Id_Type) is
   begin
      Server_Table.New_Request (Id);
   end New_Request;

   procedure Register (Server : Server_Ptr; Id : out Server_Id_Type) is
   begin
      Server_Table.Register (Server, Id);
   end Register;

   function Build_IOR
     (Type_Id : CORBA.RepositoryId;
      POA : Broca.POA.Ref;
      Key : Broca.Buffers.Encapsulation)
     return CORBA.Object.Ref
   is
      The_IOR : Broca.Object.Object_Ptr
        := new Broca.Object.Object_Type;
      The_Ref : CORBA.Object.Ref;
      Object_Key_Buffer : aliased Buffer_Type;
      Server  : Server_Ptr;
   begin
      The_IOR.Type_Id := CORBA.String (Type_Id);

      POA_Object_Of (POA).Link_Lock.Lock_R;
      --  Lock the POA.  As a result, we are sure it won't be destroyed
      --  during the marshalling of the IOR.

      --  Create Object_Key.

      --  In Broca, an object_key is an Encapsulation
      --  that contains a POA reference and an object
      --  identifier.
      begin
         Start_Encapsulation (Object_Key_Buffer'Access);
         Broca.Server.Marshall (Object_Key_Buffer'Access, POA);
         Marshall (Object_Key_Buffer'Access, Key);
      exception
         when others =>
            POA_Object_Of (POA).Link_Lock.Unlock_R;
            raise;
      end;

      declare
         Object_Key   : constant Encapsulation
           := Encapsulate (Object_Key_Buffer'Access);

         Nbr_Profiles : CORBA.Unsigned_Long;

      begin
         Release (Object_Key_Buffer);
         POA_Object_Of (POA).Link_Lock.Unlock_R;
         --  The POA should not be unlocked before Encapsulate,
         --  to allow future use of marshall-by-reference
         --  for the construction of Object_Key.

         Nbr_Profiles := 0;
         for N in Server_Id_Type loop
            Server := Server_Table.Get_Server_By_Id (N);
            exit when Server = null;
            if Can_Create_Profile (Server) then
               Nbr_Profiles := Nbr_Profiles + 1;
            end if;
         end loop;

         The_IOR.Profiles := new Broca.IOP.Profile_Ptr_Array'
           (1 .. Nbr_Profiles => null);

         for N in Server_Id_Type loop
            Server := Server_Table.Get_Server_By_Id (N);
            exit when Server = null;
            if Can_Create_Profile (Server) then
               The_IOR.Profiles (CORBA.Unsigned_Long (N))
                 := Make_Profile (Server, Object_Key);
            end if;
         end loop;

         CORBA.Object.Set (The_Ref, CORBA.Impl.Object_Ptr (The_IOR));
         return The_Ref;
      end;
   end Build_IOR;

   --  Create an ORB server.
   type This_ORB_Type is new Broca.ORB.ORB_Type with null record;
   procedure Run (ORB : in out This_ORB_Type);
   procedure POA_State_Changed
     (ORB : in out This_ORB_Type; POA : Broca.POA.Ref);

   procedure Serv;

   procedure Serv is
      Server : Server_Ptr;
   begin
      pragma Debug (O ("Starting server loop"));
      loop
         Server_Table.Get_Server (Server);
         Perform_Work (Server);
      end loop;
   exception
      when E : others =>
         pragma Debug
           (O ("ORB task Server: exception " &
               Ada.Exceptions.Exception_Name (E) & ": " &
               Ada.Exceptions.Exception_Message (E)));
         pragma Debug (O ("ORB task Server shut down"));
         null;
   end Serv;

   procedure Run (ORB : in out This_ORB_Type) is
      task type Server_Task_Type is
         pragma Storage_Size (Broca.Parameters.Server_Tasks_Storage_Size);
      end Server_Task_Type;

      task body Server_Task_Type is
      begin
         Serv;
      end Server_Task_Type;

      type Server_Task_Array is array (Natural range <>) of Server_Task_Type;
      Server_Tasks : Server_Task_Array (1 .. Broca.Flags.Nbr_Server_Tasks - 1);
      pragma Warnings (Off, Server_Tasks);
   begin
      Serv;
   end Run;

   procedure POA_State_Changed
     (ORB : in out This_ORB_Type; POA : Broca.POA.Ref)
   is
   begin
      Log ("unqueue requests");
      Queues.Hold_Queue.Unqueue_By_POA (POA);
   end POA_State_Changed;

   --  This procedure is called by a POA to request a server task to perform
   --  arbitrary work, such as cleaning the POA up.
   procedure Request_Cleanup (POA : Broca.POA.Ref) is
   begin
      Queues.Wait_Queue.Append (null, null, POA);
   end Request_Cleanup;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      POA    : in Broca.POA.Ref;
      Num    : in Natural)
   is
      The_POA : constant POA_Object_Ptr := POA_Object_Of (POA);

   begin
      if The_POA.Parent = null then

         --  The RootPOA was reached

         Marshall (Buffer, CORBA.Unsigned_Long (Num));

      else
         declare
            Parent_Ref : Broca.POA.Ref;

         begin
            Set (Parent_Ref, The_POA.Parent);
            Marshall (Buffer, Parent_Ref, Num + 1);
         end;

         Marshall (Buffer, The_POA.Name);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      POA    : in Ref)
   is
      use PortableServer;

      The_POA : constant POA_Object_Ptr := POA_Object_Of (POA);

   begin
      pragma Debug (O ("Marshall : enter"));

      --   0 ..  3: boot time or 0 if persistent POA.
      if The_POA.Lifespan_Policy = PortableServer.TRANSIENT then
         Marshall (Buffer, CORBA.Unsigned_Long'(0));

      else
         Marshall (Buffer, Broca.Flags.Boot_Time);
      end if;

      --   4 ..  7: poa index in all_POAs
      Marshall (Buffer, CORBA.Unsigned_Long (The_POA.Index));

      --   8 .. 11: date of the entry
      Marshall (Buffer, CORBA.Unsigned_Long (All_POAs (The_POA.Index).Date));

      --  12 .. 15: N_POA

      --  number of POAs in the POA path name, can be 0 if POA is
      --  TRANSIENT. The POA path name starts after the RootPOA, ie
      --  for an object of the RootPOA, its path name is empty.

      if The_POA.Lifespan_Policy = PortableServer.TRANSIENT then
         Marshall (Buffer, CORBA.Unsigned_Long'(0));

      else
         Marshall (Buffer, POA, 0);
      end if;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer    : access Buffer_Type;
      POA       : out Broca.POA.Ref;
      POA_State : out Broca.POA.Processing_State_Type)
   is
      Current_POA   : Broca.POA.Ref;
      Old_POA       : Broca.POA.Ref;
      Tmp_POA_State : Broca.POA.Processing_State_Type;

      POA_Name  : CORBA.String;
      Path_Size : CORBA.Unsigned_Long;
      POA_Date  : Natural;
      POA_Index : Broca.POA.POA_Index_Type;
      Boot_Time : CORBA.Unsigned_Long;

   begin
      pragma Debug (O ("Unmarshall : enter"));

      --  Unmarshall boot time.
      Boot_Time := Unmarshall (Buffer);
      if Boot_Time /= 0 and then Boot_Time /= Broca.Flags.Boot_Time then
         pragma Debug (O ("Incorrect boot time"));
         pragma Debug (O ("Local  boot time is" & Broca.Flags.Boot_Time'Img));
         pragma Debug (O ("Remote boot time is" & Boot_Time'Img));
         Broca.Exceptions.Raise_Object_Not_Exist;
      end if;

      --  Unmarshall POA index.
      POA_Index := POA_Index_Type (CORBA.Unsigned_Long'(Unmarshall (Buffer)));
      pragma Debug (O ("POA index =" & POA_Index'Img));

      --  Unmarshall POA date.
      POA_Date := Natural (CORBA.Unsigned_Long'(Unmarshall (Buffer)));
      pragma Debug (O ("POA date =" & POA_Date'Img));

      --  Lock the table, so that we are sure the poa won't be destroyed.
      Broca.POA.All_POAs_Lock.Lock_R;

      if All_POAs /= null
        and then POA_Index in All_POAs.all'Range
        and then All_POAs (POA_Index).Date = POA_Date
      then
         POA := All_POAs (POA_Index).POA;

         --  Neither the POA won't be destroyed, nor its children.
         POA_Object_Of (POA).Link_Lock.Lock_R;
         All_POAs_Lock.Unlock_R;

         --  Just skip the path name.
         --  Unmarshall number of POAs in path name.
         Path_Size := Unmarshall (Buffer);

         for I in 1 .. Path_Size loop

            --  Just skip over the path names

            declare
               N : constant CORBA.String := Unmarshall (Buffer);
               pragma Warnings (Off, N);
            begin
               pragma Debug (O ("Skip POA name: " & To_Standard_String (N)));
               null;
            end;
         end loop;

         Inc_Usage_If_Active
           (Get_The_POAManager (POA_Object_Of (POA)).all, POA_State);

      else
         --  Need to be revisited.

         --  Unmarshall number of POAs in path name.
         Path_Size := Unmarshall (Buffer);

         --  Start with the root POA
         Current_POA := All_POAs (Broca.POA.Root_POA_Index).POA;

         --  Neither the POA nor its children won't be destroyed
         POA_Object_Of (Current_POA).Link_Lock.Lock_R;
         Broca.POA.All_POAs_Lock.Unlock_R;

         for I in 1 .. Path_Size loop
            Inc_Usage_If_Active
              (Get_The_POAManager (POA_Object_Of (Current_POA)).all,
               Tmp_POA_State);

            if Tmp_POA_State /= Active then
               POA := Current_POA;
               POA_State := Tmp_POA_State;
               return;
            end if;

            Dec_Usage (Get_The_POAManager (POA_Object_Of (Current_POA)).all);

            POA_Name := Unmarshall (Buffer);
            Old_POA  := Current_POA;
            Current_POA := Broca.POA.Ref
              (Broca.POA.Find_POA
               (POA_Object_Of (Current_POA), POA_Name, True));

            if Is_Nil (Current_POA) then
               POA_Object_Of (Old_POA).Link_Lock.Unlock_R;
               Broca.Exceptions.Raise_Object_Not_Exist;
            end if;

            POA_Object_Of (Current_POA).Link_Lock.Lock_R;
            POA_Object_Of (Old_POA).Link_Lock.Unlock_R;
         end loop;

         POA := Current_POA;

         Inc_Usage_If_Active
           (Get_The_POAManager (POA_Object_Of (Current_POA)).all,
            POA_State);

         --  FIXME: should set the new index, the new date and raise
         --  location forward.

      end if;
   end Unmarshall;

begin
   Broca.ORB.Register_ORB
     (new This_ORB_Type'(Broca.ORB.ORB_Type with null record));
end Broca.Server;
