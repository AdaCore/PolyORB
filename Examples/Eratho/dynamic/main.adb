with Controller; use Controller;
pragma Elaborate (Controller);

with Prime;
pragma Elaborate (Prime);

with Text_IO; use Text_IO;
with Common; use Common;

procedure Main is
   Divider : Natural;
   Where   : Natural;
   First   : Prime_Pool_Access;
begin
   First := Controller.First;
   for Number in 2 .. 50 loop
      Test_Number (First, Number, Divider, Where);
      if Divider = Number then
	      Put_Line (Number'Img & " (prime on" & Where'Img & ")");
      else
         Put_Line (Number'Img & "             (divided by" &
                   Divider'Img & " on" & Where'Img & ")");
      end if;
   end loop;
end Main;
