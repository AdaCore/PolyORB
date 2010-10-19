with Types; use Types;
package Alarm is

   pragma Pure;
   type Terminal_Type is abstract tagged limited private;

   procedure Notify
     (Terminal : access Grant_Type;
      Donator  : in Customer_Type;
      Amount   : in Integer) is abstract;

private
   type Grant_Type is abstract tagged limited null record;
end Notify;
