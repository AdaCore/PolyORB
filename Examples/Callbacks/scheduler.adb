with Types;   use Types;
with Text_IO; use Text_IO;
package body Scheduler is

   protected Lock is
      entry Enter;
      procedure Leave;
   private
      Ready : Boolean := True;
   end Lock;

   protected body Lock is
      entry Enter when Ready is
      begin
         Ready := False;
      end Enter;
      procedure Leave is
      begin
         Ready := True;
      end Leave;
   end Lock;

   Workers : array (1 .. 5) of Any_Worker;
   W_Last  : Natural := 0;

   Q_Max   : constant Query := 9;

   Replies : array (1 .. Q_Max) of Reply := (others => No_Reply);
   R_Last  : Reply := 0;
   Q_Last  : Query := 0;

   procedure Save (Q : Query; R : Reply; P : Natural) is
   begin
      Lock.Enter;
      Replies (Q) := R;
      R_Last := R_Last + 1;
      Lock.Leave;
      Put_Line ("Query" & Q'Img & " executed on Partition" & P'Img);
   end Save;

   function Continue return Boolean is
   begin
      return R_Last /= Reply (Q_Max);
   end Continue;

   procedure Push (W : Any_Worker) is
   begin
      Lock.Enter;
      W_Last := W_Last + 1;
      Workers (W_Last) := W;
      Lock.Leave;
   end Push;

   procedure Pop  (W : out Any_Worker; Q : out Query) is
   begin
      Lock.Enter;
      if W_Last > 0 and then Q_Last /= Q_Max then
         W := Workers (W_Last);
         W_Last := W_Last - 1;
         Q_Last := Q_Last + 1;
         Q := Q_Last;
      else
         W := null;
         Q := No_Query;
      end if;
      Lock.Leave;
   end Pop;

end Scheduler;
