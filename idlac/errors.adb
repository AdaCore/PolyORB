with Ada.Text_Io;

package body Errors is

   --  counters for errors and warnings
   Error_Count : Natural := 0;
   Warning_Count : Natural := 0;

   --  nice display of a natural
   function Nat_To_String (Val : Natural) return String is
      Res : String := Natural'Image (Val);
   begin
      return Res (2 .. Res'Last);
   end Nat_To_String;

   --  adds a new error
   procedure Display_Error (Message : in String;
                            Line : in Natural;
                            Column : in Positive;
                            Kind : in Error_Kind) is
   begin
      if Kind = Error then
         Error_Count := Error_Count + 1;
      else
         Warning_Count := Warning_Count + 1;
      end if;
      if Kind = Error then
         Ada.Text_Io.Put ("ERROR occured at line ");
      elsif Kind = Error then
         Ada.Text_Io.Put ("WARNING occured at line ");
      end if;
      Ada.Text_Io.Put (Nat_To_String (Line));
      Ada.Text_Io.Put (", column ");
      Ada.Text_Io.Put (Nat_To_String (Column));
      Ada.Text_Io.Put_Line (" : ");
      Ada.Text_Io.Put ("    ");
      Ada.Text_Io.Put_Line (Message);
   end Display_Error;

   --  was there any errors ?
   function Is_Error return Boolean is
   begin
      return Error_Count > 0;
   end Is_Error;

   --  was there any warnings ?
   function Is_Warning return Boolean is
   begin
      return Warning_Count > 0;
   end Is_Warning;

   --  returns the number of warnings
   function Warning_Number return Natural is
   begin
      return Error_Count;
   end Warning_Number;

   --  returns the number of errors
   function Error_Number return Natural is
   begin
      return Warning_Count;
   end Error_Number;

end Errors;
