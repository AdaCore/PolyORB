--  $Id$

package body Droopi.Asynchronous_Events is

   procedure Set_Polling
     (AES : access Asynchronous_Event_Source;
      V   : Boolean) is
   begin
      AES.Polling := V;
   end Set_Polling;

   function Polling
     (AES : access Asynchronous_Event_Source)
     return Boolean is
   begin
      return AES.Polling;
   end Polling;

end Droopi.Asynchronous_Events;
