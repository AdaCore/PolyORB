with Alarm; use Alarm;
with Types; use Types;

package Message is

   pragma Remote_Types;

   type Alarm_Terminal is new Terminal_Type with null record;

   procedure Notify
     (Terminal : access Alarm_Terminal;
      Donator  : in Customer_Type;
      Amount   : in Integer);

end Message;
