with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

procedure B_GaPrCo is
   Up_To_Low : constant Natural := Character'Pos ('a') - Character'Pos ('A');
   Protocol  : String := Argument (1);
   SGP       : constant String := "System.Garlic.Protocols";

begin
   if Protocol (1) in 'a' .. 'z' then
      Protocol (1) := Character'Val (Character'Pos (Protocol (1)) - Up_To_Low);
   end if;
   for I in 2 .. Protocol'Last loop
      if Protocol (I) in 'A' .. 'Z' then
         Protocol (I)
           := Character'Val (Character'Pos (Protocol (I)) + Up_To_Low);
      end if;
   end loop;
   declare
      P : constant String := SGP & "." & Protocol;
      S : constant String := P & ".Server";
   begin
      Ada.Text_IO.Put_Line ("with " & P & ";");
      Ada.Text_IO.Put_Line ("with " & S & ";");
      Ada.Text_IO.Put_Line ("pragma Elaborate_All (" & S & ");");
      Ada.Text_IO.Put_Line ("pragma Warnings (Off, " & S & ");");
      Ada.Text_IO.Put_Line ("package body " & SGP & ".Config is");
      Ada.Text_IO.Put_Line ("   procedure Initialize is");
      Ada.Text_IO.Put_Line ("   begin");
      Ada.Text_IO.Put_Line ("      Register (" & P & ".Create);");
      Ada.Text_IO.Put_Line ("   end Initialize;");
      Ada.Text_IO.Put_Line ("end " & SGP & ".Config;");
   end;

end B_GaPrCo;
