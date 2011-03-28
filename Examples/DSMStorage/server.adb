with GNAT.IO; use GNAT.IO;
with Vector;  use Vector;
with Scheduler;
with Client;

procedure Server is
begin
   Client;

   Put ("(");
   for I in Content_Type'Range loop
      Put (Content.Value (I)'Img);
      if I /= Content_Type'Last then
         Put (",");
      end if;
   end loop;
   Put_Line (")");
end Server;
