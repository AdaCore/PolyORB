with Types; use Types;
with Text_Io; use Text_Io;
with Alarm; use Alarm;

package body Message is

   procedure Notify
     (Terminal : access Alarm_Terminal;
      Donator  : in Customer_Type;
      Amount   : in Integer) is
   begin
      New_Line;
      New_Line;
      Put ("=> Receive");
      Put (Integer'Image (Amount));
      Put (" from ");
      Put (String (Donator));
      New_Line;
   end Notify;

end Message;
