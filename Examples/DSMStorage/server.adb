with GNAT.IO; use GNAT.IO;
with Vector;  use Vector;
with Scheduler;
with Client;

procedure Server is
begin
   Client;

   Put ("(");
   for I in Content'Range loop
      Put (Content (I)'Img);
      if I /= Content'Last then
         Put (",");
      end if;
   end loop;
   Put_Line (")");
end Server;
