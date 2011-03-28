with System.IO;
package body Spif.IO is

   procedure Put
     (Msg  : in String;
      Onto : in IO_Device := Terminal) is
   begin
      System.IO.Put (Msg);
   end Put;

   procedure Put_Line
     (Msg  : in String;
      Onto : in IO_Device := Terminal) is
   begin
      System.IO.Put_Line (Msg);
   end Put_Line;

   procedure New_Line
     (Onto : in IO_Device := Terminal) is
   begin
      System.IO.New_Line;
   end New_Line;

   function Get_Line return String is
   begin
      return "Not yet implemented";
   end Get_Line;

   function Get_Character return Character is
   begin
      return '*';
   end Get_Character;

end Spif.IO;
