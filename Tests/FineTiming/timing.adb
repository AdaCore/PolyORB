--
--  $Id$
--

with Ada.Real_Time; use Ada.Real_Time;

package body Timing is

   Start_Time : constant Time := Clock;

   -------------
   -- Current --
   -------------

   function Current return Milliseconds is
      Expired : constant Time_Span := Clock - Start_Time;
   begin
      return Milliseconds (To_Duration (Expired) * 1000);
   end Current;

end Timing;
