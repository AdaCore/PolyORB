with Ada.Command_Line;   use Ada.Command_Line;
with Ada.Text_IO;        use Ada.Text_IO;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Interfaces.C;       use Interfaces.C;
with System.Garlic.Thin; use System.Garlic.Thin;

-- Is Thread Blocking IO

procedure IsThrBIO is

   Process_Blocking_IO : Boolean;

   task A_Task is
      entry Start;
      entry Stop;
   end A_Task;

   task body A_Task is
   begin
      accept Start;
      delay 0.1;
      select
         delay 0.1;
         Process_Blocking_IO := False;
      or
         accept Stop;
         Process_Blocking_IO := True;
      end select;
      select
         accept Stop;
      or
         terminate;
      end select;
   end A_Task;

   Result  : int;
   Input   : Fd_Set_Access := new Fd_Set'(0);

   Timeout : Timeval_Access := new Timeval'(1, 0);

   File    : File_Type;

begin
   A_Task.Start;
   Result := C_Select (1, Input, null, null, Timeout);
   A_Task.Stop;
   Create (File, Out_File, Argument (1));
   if Process_Blocking_IO then
      Put_Line (File, "with System.Garlic.Non_Blocking;");
   else
      Put_Line (File, "with System.Garlic.Thin;");
   end if;
   Put (File, "package System.Garlic.TCP.Operations renames ");
   if Process_Blocking_IO then
      Put_Line (File, "System.Garlic.Non_Blocking;");
   else
      Put_Line (File, "System.Garlic.Thin;");
   end if;
   Close (File);
   if Process_Blocking_IO then
      OS_Exit (0);
   else
      OS_Exit (1);
   end if;
end IsThrBIO;
