with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Normal_JSON; use Normal_JSON;

with RCI;

procedure Cli is
   J1 : JSON_Value := Create_Object;
   J2 : JSON_Value;
begin
   J1.Set_Field ("data", Create (123));
   Put_Line ("J1 = " & Write (J1));

   J2 := Unwrap (RCI.Frob (Wrap (J1)));
   Put_Line ("J2 = " & Write (J2));
end Cli;
