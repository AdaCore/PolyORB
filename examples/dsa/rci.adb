with Ada.Text_IO;
package body RCI is

   procedure My_Proc (X : in Integer; Y : in out Predicate; Z : out Trit) is
   begin
      Y := Y and then Predicate (X = 0);
      Z := 1;
   end My_Proc;

   function My_Func (S : String) return Color is
      pragma Unreferenced (S);
   begin
      return Blue;
   end My_Func;

   function echoString (S : String) return String is
   begin
      Ada.Text_IO.Put_Line ("Thus spake my DSA client unto me: « "
        & S & " ».");
      return "You said: « " & S & " »";
   end echoString;

end RCI;
