--  Abstract data type for an asynchrous event source.

--  $Id$

with Ada.Unchecked_Deallocation;

package body PolyORB.Asynch_Ev is

   function Notepad_Of (AES : Asynch_Ev_Source_Access)
     return Annotations.Notepad_Access is
   begin
      return AES.Notes'Access;
   end Notepad_Of;

   procedure Unregister_Source
     (AES : Asynch_Ev_Source_Access) is
   begin
      pragma Assert (AES /= null and then AES.Monitor /= null);
      Unregister_Source (AES.Monitor.all, AES);
   end Unregister_Source;

   procedure Destroy
     (AES : in out Asynch_Ev_Source_Access)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation
        (Asynch_Ev_Source'Class, Asynch_Ev_Source_Access);
   begin
      Free (AES);
   end Destroy;

end PolyORB.Asynch_Ev;
