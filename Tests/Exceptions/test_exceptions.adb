--
--  test_exceptions.adb,v 1.2 1996/04/10 15:42:53 tardieu Exp
--

with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C;
with Exceptions_Remote; use Exceptions_Remote;
with Text_IO; use Text_IO;

procedure Test_Exceptions is

   Program_Erro : exception;

begin
   begin
      Raise_Program_Error;
   exception
      when Error : Program_Error =>
         Put_Line ("Program error got raised with message: " &
                   Exception_Message (Error));
      when Trunc : Program_Erro =>
         Put_Line ("Truncated exception Program_Erro with message: " &
                   Exception_Message (Trunc));
      when Bogus : others =>
         Put_Line ("Bogus: " & Exception_Name (Bogus) & " raised");
   end;
   begin
      Raise_Constraint_Error;
   exception
      when Error : Constraint_Error =>
         Put_Line ("Constraint error got raised with message: " &
                   Exception_Message (Error));
      when Bogus : others =>
         Put_Line ("Bogus: " & Exception_Name (Bogus) & " raised");
   end;
   begin
      Raise_No_Error;
      Put_Line ("Ok, no error");
   exception
      when Bogus : others =>
         Put_Line ("Bogus: " & Exception_Name (Bogus) & " raised");
   end;
   begin
      Raise_User_Error;
   exception
      when Error : User_Error =>
         Put_Line ("User error got raised with message: " &
                   Exception_Message (Error));
      when Bogus : others =>
         Put_Line ("Bogus: " & Exception_Name (Bogus) & " raised");
         Put_Line ("with message " & Exception_Message (Bogus));
   end;
end Test_Exceptions;
