--  $Id$

with Unchecked_Deallocation;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Locked_Queue is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log
     ("locked_queue");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new Unchecked_Deallocation
     (Queue_Node, Queue_Node_Access);

   ----------
   -- Lock --
   ----------

   protected body Lock is
      entry Enter when not Taken is
      begin
         Taken := True;
      end Enter;

      procedure Leave is
      begin
         Taken := False;
      end Leave;
   end Lock;

   ------------
   -- Create --
   ------------

   procedure Create
     (Result    :    out Queue;
      Max_Count : in     Positive)
   is
   begin
      Result.Max_Count := Max_Count;
      Result.Empty_Lock.Enter;
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add
     (Q : in out Queue;
      E : in     Queue_Element)
   is
   begin
      pragma Debug (O ("adding"));
      Q.Full_Lock.Enter;

      Q.State_Lock.Enter;
      if Q.Last = null then
         Q.Last := new Queue_Node'
           (Element => new Queue_Element'(E),
            Next    => null);
         Q.First := Q.Last;
      else
         Q.Last.Next := new Queue_Node'
           (Element => new Queue_Element'(E),
            Next    => null);
         Q.Last := Q.Last.Next;
      end if;
      Q.Count := Q.Count + 1;
      Q.State_Lock.Leave;

      if Q.Count = 1 then
         pragma Debug (O ("unlocked empty lock"));
         Q.Empty_Lock.Leave;
      end if;

      if Q.Count /= Q.Max_Count then
         Q.Full_Lock.Leave;
      end if;
   end Add;

   --------------
   -- Get_Head --
   --------------

   procedure Get_Head
     (Q : in out Queue;
      E :    out Queue_Element)
   is
   begin
      pragma Debug (O ("trying to get empty lock"));
      Q.Empty_Lock.Enter;
      pragma Debug (O ("got empty lock"));

      --  When execution reaches this, necessarily Q.First /= null.

      Q.State_Lock.Enter;

      declare
         --  Old_First : Queue_Node_Access := Q.First;
      begin
         E := Q.First.Element.all;
         Q.First := Q.First.Next;

         --  ??? Should free old elements !!
         --  Free (Old_First);
      end;

      Q.Count := Q.Count - 1;
      Q.State_Lock.Leave;

      --  When execution reaches this, necessarily the queue is not full.

      if Q.Count = Q.Max_Count - 1 then
         Q.Full_Lock.Leave;
      end if;

      if Q.Count > 0 then
         Q.Empty_Lock.Leave;
      end if;

   end Get_Head;

end Locked_Queue;
