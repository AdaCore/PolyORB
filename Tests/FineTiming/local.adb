--
--  $Id$
--

with Ada.Text_IO; use Ada.Text_IO;
with Remote; use Remote;
with Timing; use Timing;

procedure Local is

   Buffer : constant T := (others => 1);

   Before, After : Milliseconds;

begin
   Put_Line ("Testing communication");
   Synchronous_Empty_Test;

   Put ("Starting AET... "); Flush;
   Before := Current;
   for I in 1 .. 50 loop
      Asynchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Line ("AET summary:" &
             Milliseconds'Image ((After - Before) / 50) & "ms");

   Put ("Starting AT... ");
   Before := Current;
   for I in 1 .. 50 loop
      Asynchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Line ("AT summary:" &
             Milliseconds'Image ((After - Before) / 50) & "ms");

   Put ("Starting SET... ");
   Before := Current;
   for I in 1 .. 50 loop
      Synchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Line ("SET summary:" &
             Milliseconds'Image ((After - Before) / 50) & "ms");

   Put ("Starting ST... ");
   Before := Current;
   for I in 1 .. 50 loop
      Synchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Line ("ST summary:" &
             Milliseconds'Image ((After - Before) / 50) & "ms");
end Local;
