with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Sema;
with Report; use Report;

procedure Client is
   IOR    : CORBA.String;
   MySema : Sema.Ref;

   protected Status is
      procedure Set_Success;
      procedure Set_Failure;
      procedure Get (S, F : out Natural);
   private
      Success : Natural := 0;
      Failure : Natural := 0;
   end Status;

   protected body Status is
      procedure Set_Success is
      begin
         Success := Success + 1;
      end Set_Success;

      procedure Set_Failure is
      begin
         Failure := Failure + 1;
      end Set_Failure;

      procedure Get (S, F : out Natural) is
      begin
         S := Success;
         F := Failure;
      end Get;
   end Status;

   task type Agent is
      entry Start (S : Sema.Ref);
      entry Stop;
   end Agent;

   task body Agent is
      MySema : Sema.Ref;
   begin
      accept Start (S : Sema.Ref) do
         MySema := S;
      end Start;

      for I in 1 .. 10 loop
         begin
            Sema.Try_P (MySema, 1);
            Status.Set_Success;
         exception when E : Sema.Empty =>
            declare
               M : Sema.Empty_Members;
            begin
               Sema.Get_Members (E, M);
               if M.Missing = 1 then
                  Status.Set_Failure;
               end if;
            end;
         end;
         delay 0.1;
      end loop;
      accept Stop;
   end Agent;

   Agents : array (Natural range 1 .. 10) of Agent;

   task Blocking_Task is
      entry Start (S : Sema.Ref);
      entry Complete;
      entry Release;
      entry Stop  (N : out Natural);
   end Blocking_Task;

   task body Blocking_Task is
      MySema : Sema.Ref;
      Number : Natural := 0;
   begin
      accept Start (S : Sema.Ref) do
         MySema := S;
      end Start;
      loop
         select
            accept Release do
               Sema.V (MySema, 1);
               Number := Number + 1;
            end Release;
         or
            accept Complete;
            exit;
         end select;
      end loop;
      accept Stop (N : out Natural) do
         N := Number;
      end Stop;
   end Blocking_Task;
  
   task Blocked_Task is
      entry Start (S : Sema.Ref);
      entry Stop;
   end Blocked_Task;

   task body Blocked_Task is
      MySema : Sema.Ref;
   begin
      accept Start (S : Sema.Ref) do
         MySema := S;
      end Start;
      Sema.P (MySema, 10);
      Blocking_Task.Complete;
      accept Stop;
   end Blocked_Task;
  
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MySema);

   if Sema.Is_Nil (MySema) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   for I in Agents'Range loop
      Agents (I).Start (MySema);
   end loop;   
   for I in Agents'Range loop
      Agents (I).Stop;
   end loop;   
   
   declare
      Success : Natural := 0;
      Failure : Natural := 0;
   begin
      Status.Get (Success, Failure);
      Output ("parallel operations and exceptions",
              Success = 20 and then Failure = 80);
   end;

   Blocking_Task.Start (MySema);
   Blocked_Task.Start (MySema);
   for I in 1 .. 10 loop
      Blocking_Task.Release;
      delay 0.1;
   end loop;
   declare
      N : Natural := 0;
   begin
      Blocking_Task.Stop (N);
      Output ("waiting for remote guard", N = 10);
   end;
   Blocked_Task.Stop;
end Client;
