--
--  $Id$
--

with Ada.Text_IO; use Ada.Text_IO;
with Remote;      use Remote;
with Timing;      use Timing;

procedure Local is

   Buffer : constant T := (others => 1);

   Before, After : Milliseconds;

   Iterations : constant := 5000;

   package FIO is new Float_IO (Float);
   use FIO;

   procedure Put_Summary (T : in String);

   -----------------
   -- Put_Summary --
   -----------------

   procedure Put_Summary (T : in String) is
   begin
      Put_Line ("done");
      Put ("                   " & T & " summary:");
      Put (Float (After - Before) / Float (Iterations), Fore => 3, Aft => 2,
           Exp => 0);
      Put_Line ("ms");
   end Put_Summary;

begin
   Put ("Testing communication...");
   Flush;
   Synchronous_Empty_Test;
   Put_Line (" OK");

   Put ("Starting AET... "); Flush;
   Before := Current;
   for I in 1 .. Iterations loop
      Asynchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Summary ("AET");

   Put ("Starting AT... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Asynchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Summary ("AT");

   Put ("Starting SET... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Synchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Summary ("SET");

   Put ("Starting ST... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Synchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Summary ("ST");

end Local;
