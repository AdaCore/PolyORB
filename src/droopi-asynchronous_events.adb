--  Abstract data type for an asynchrous event source.

--  $Id$

package body Droopi.Asynchronous_Events is

   function Notepad_Of (AES : Asynchronous_Event_Source_Access)
     return Annotations.Notepad_Access is
   begin
      return AES.Notes'Access;
   end Notepad_Of;

   procedure Unregister_Source
     (AES : Asynchronous_Event_Source_Access) is
   begin
      pragma Assert (AES /= null and then AES.Monitor /= null);
      Unregister_Source (AES.Monitor.all, AES);
   end Unregister_Source;

end Droopi.Asynchronous_Events;
