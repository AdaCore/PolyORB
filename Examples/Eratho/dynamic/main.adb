with Controller; use Controller;
pragma Elaborate (Controller);

with Prime;
pragma Elaborate (Prime);

with Text_IO; use Text_IO;
with Common; use Common;

procedure Main is
   Prime   : Natural;
   Initial : Natural := 0;
   First   : Prime_Pool_Access;
begin
   First := Controller.First;
   for Number in 2 .. 50 loop
      Test_Number (First, Number, Initial, Prime);
      if Prime = Number then
	      Put_Line (Number'Img & " (prime)");
      else
         Put_Line (Number'Img & "         (divided by" & Prime'Img & ")");
      end if;
   end loop;
end Main;
