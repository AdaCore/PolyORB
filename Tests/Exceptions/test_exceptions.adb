--
--  test_exceptions.adb,v 1.2 1996/04/10 15:42:53 tardieu Exp
--

with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C;
with Exceptions_Remote; use Exceptions_Remote;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Exceptions is

   procedure Print_Exception (E : in Exception_Occurrence) is
      Name : constant String := Exception_Name (E);
   begin
      Put_Line ("Exception name length:" & Natural'Image (Name'Length));
      for I in Name'Range loop
         Put_Line ("Char" & Positive'Image (I) & " is """ &
                   Name (I) & """" & Natural'Image (Character'Pos (Name (I))));
      end loop;
   end Print_Exception;

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
         Print_Exception (Bogus);
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
   begin
      Put_Line ("Trying to raise Hidden_User_Error");
      Raise_Hidden_User_Error;
   exception
      when Error : others =>
         Put_Line (Exception_Name (Error) & " raised");
         Put_Line ("with message: " & Exception_Message (Error));
   end;
end Test_Exceptions;
