--
--  $Id$
--

with Ada.Text_IO; use Ada.Text_IO;
with System.Garlic.Heart; use System.Garlic.Heart;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Startup; use System.Garlic.Startup;
with System.Garlic.Termination; use System.Garlic.Termination;

procedure Test_Startup is

begin
   Put_Line ("Boot server location: " & Get_Boot_Server);
   Put_Line ("My location: " & To_String (Get_My_Location));
   Put_Line ("Active tasks:" & Natural'Image (Get_Active_Task_Count));
end Test_Startup;
