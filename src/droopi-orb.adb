--  $Id$

with Droopi.ORB.Task_Policies;
with Droopi.Soft_Links;
with Droopi.Channels;

package body Droopi.ORB is

   use Droopi.Jobs;
   use Droopi.Sockets;
   use Droopi.Soft_Links;
   use Droopi.ORB.Task_Policies;

   type ORB is new Root_ORB with record
      Tasking_Policy : Task_Policies.Tasking_Policy_Access;
   end record;

   --  Overridden primitive operations of ORB:

   procedure Handle_Event (O : access ORB; AS : Active_Socket);

   function Work_Pending (O : access ORB) return Boolean;

   procedure Perform_Work (O : access ORB);

   procedure Run
     (O              : access ORB;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False);

   procedure Shutdown
     (O : access ORB;
      Wait_For_Completion : Boolean := True);

   procedure Insert_Socket
     (O  : access ORB;
      AS : Active_Socket);

   procedure Delete_Socket (O : access ORB; S : Socket_Type);

   ------------------------------
   -- Server object operations --
   ------------------------------

   procedure Handle_Event (O : access ORB; AS : Active_Socket) is
   begin
      case AS.Kind is
         when Listening_SK =>

            --  A new connection.

            declare
               New_AS : Active_Socket
                 := (Kind     => Communication_Sk,
                     Socket   => Null_Socket,
                     Session  => Protocols.Create_Session
                       (AS.Protocol),
                     Protocol => AS.Protocol);
               Addr : Sock_Addr_Type;
            begin
               Accept_Socket
                 (Server  => AS.Socket,
                  Socket  => New_AS.Socket,
                  Address => Addr);
               --  The call to Accept_Socket must not block,
               --  and must return a valid socket.

               Handle_New_Connection
                 (O.Tasking_Policy, ORB_Access (O), New_AS);

               --  Insert connection in list of active connections.
               --  If the threading policy is "thread-per-session",
               --  a new specific task is created which will handle all
               --  messages on that connection, else the associated channel
               --  is inserted in the set of channels that are monitored
               --  through select() by general-purpose ORB tasks.
            end;

         when Communication_SK =>

            --  Data arrived on a communication channel.

            declare
               Channel : Droopi.Channels.Channel_Access
                 := null;
            begin
               --  XXX Initialize Channel to the channel corresponding to
               --  active socket AS.
               Droopi.Channels.Handle_Data (Channel, AS);

               --  Signal upper layers that data is available on this
               --  channel. Further processing and possible tasking decisions
               --  are delegated to the upstream protocol, since they may depend
               --  upon the particular messages received.

            end;

         when Invalid_SK =>

            --  An error condition (AS is not a valid active socket descriptor).

            pragma Assert (False);
            null;
      end case;
   end Handle_Event;

   function Create_ORB return ORB_Access is
      O : constant ORB_Access := new ORB;
   begin
      Create (O.Idle_Tasks);

      O.Job_Queue := Droopi.Jobs.Create_Queue;
      O.Shutdown := False;
      O.Polling  := False;

      return O;
   end Create_ORB;

   procedure Start (O : access ORB);

   procedure Start (O : access ORB) is
   begin
      --  Start accepting incoming connections.
      --  XXX TBD
      raise Not_Implemented;
   end Start;

   function Try_Perform_Work (Q : access Job_Queue) return Boolean;
   --  Perform one item of work from Q, if available.
   --  Precondition: This function must be called from within a
   --    critical section.
   --  Postcondition: if a job has been executed, then the critical
   --    section has been left, and True is returned.
   --    If no job was available, the critical section is not left,
   --    and False is returned.

   function Try_Perform_Work (Q : access Job_Queue) return Boolean is
   begin
      if not Empty (Q) then
         declare
            Job : Job_Access := Fetch_Job (Q);
         begin
            Leave_Critical_Section;

            pragma Assert (Job /= null);
            Run (Job);
            return True;
         end;
      else
         return False;
      end if;
   end Try_Perform_Work;

   procedure Run
     (O              : access ORB;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False) is
   begin
      loop
         Enter_Critical_Section;

         if (Exit_Condition /= null and then Exit_Condition.all)
           or else O.Shutdown then
            Leave_Critical_Section;
            exit;
         end if;

         if Try_Perform_Work (O.Job_Queue) then
            null;
         elsif May_Poll then

            declare
               Monitored_Set : constant Sock_Seqs.Element_Array
                 := Sock_Seqs.To_Element_Array (O.ORB_Sockets);

               R_Set : Socket_Set_Type;
               W_Set : Socket_Set_Type;
               Status : Selector_Status;

            begin
               O.Polling := True;
               Leave_Critical_Section;

               for I in Monitored_Set'Range loop
                  Set (R_Set, Monitored_Set (I).Socket);
               end loop;
               Empty (W_Set);

               if O.Selector = null then
                  Create_Selector (O.Selector);
               end if;
               pragma Assert (O.Selector /= null);

               Check_Selector
                 (Selector     => O.Selector,
                  R_Socket_Set => R_Set,
                  W_Socket_Set => W_Set,
                  Status       => Status);

               Enter_Critical_Section;
               O.Polling := False;
               Leave_Critical_Section;

               for I in Monitored_Set'Range loop
                  if Is_Set (R_Set, Monitored_Set (I).Socket) then
                     Handle_Event (O, Monitored_Set (I));
                  end if;

               end loop;
            end;
         else

            --  This task is going idle.
            --  It should first ask for persmission to
            --  do so from the tasking policy object.

            --  XX TBD
            raise Not_Implemented;

         end if;
      end loop;
   end Run;

   function Work_Pending (O : access ORB) return Boolean
   is
      Result : Boolean;
   begin
      Enter_Critical_Section;
      Result := not Empty (O.Job_Queue);
      Leave_Critical_Section;
      return Result;
   end Work_Pending;

   procedure Perform_Work (O : access ORB) is
   begin
      Enter_Critical_Section;
      if not Try_Perform_Work (O.Job_Queue) then
         Leave_Critical_Section;
      end if;
   end Perform_Work;

   procedure Shutdown
     (O                   : access ORB;
      Wait_For_Completion : Boolean := True) is
   begin

      --  Stop accepting incoming connections.
      --  XX TBD
      raise Not_Implemented;

      if Wait_For_Completion then
         --  XX TBD
         raise Not_Implemented;
      end if;

      Enter_Critical_Section;
      O.Shutdown := True;
      Leave_Critical_Section;

   end Shutdown;

   procedure Insert_Socket
     (O  : access ORB;
      AS : Active_Socket) is
   begin
      pragma Assert (AS.Kind /= Invalid_SK);

      Enter_Critical_Section;
      Sock_Seqs.Append (O.ORB_Sockets, AS);

      if O.Polling then
         Abort_Selector (O.Selector);
      end if;
      Leave_Critical_Section;
   end Insert_Socket;

   procedure Delete_Socket (O : access ORB; S : Socket_Type) is
   begin
      Enter_Critical_Section;

      declare
         Sockets : constant Sock_Seqs.Element_Array
           := Sock_Seqs.To_Element_Array (O.ORB_Sockets);
      begin
     All_Sockets:
         for I in Sockets'Range loop
            if Sockets (I).Socket = S then
               Sock_Seqs.Delete
                 (Source  => O.ORB_Sockets,
                  From    => 1 + I - Sockets'First,
                  Through => 1 + I - Sockets'First);

               exit All_Sockets;
            end if;
         end loop All_Sockets;

         --  Tried to delete a socket that is not part
         --  of the monitored set!

         pragma Assert (False);
      end;

      if O.Polling then
         Abort_Selector (O.Selector);
      end if;
      Leave_Critical_Section;
   end Delete_Socket;

end Droopi.ORB;
