--
--  $Id$
--
--  This program tries to determine if a given platform supports
--  thread-blocking IOs.
--
--  The command line to use is: gnatmake -a test_threads
--

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System.Garlic.Thin; use System.Garlic.Thin;

procedure Test_Threads is

   FD    : aliased Two_Int;
   Dummy : int;

   task Writer;
   task body Writer is
   begin
      delay 5.0;
      Dummy := C_Write (FD (1), New_String ("abcde"), 1);
   end Writer;

   Buffer : constant chars_ptr := New_String ("abcde");

begin
   Dummy := C_Pipe (FD'Access);
   Put_Line ("If the following test hangs more than 20 seconds, then kill");
   Put_Line ("it and assume the answer is 'This platform does *NOT* support");
   Put_Line ("thread-blocking IOs'");
   Dummy := C_Read (FD (0), Buffer, 1);
   Put_Line ("This platform *DOES* probably support thread-blocking IOs");
end Test_Threads;
