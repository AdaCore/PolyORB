----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with TVTForward.second;
with TVTForward.first;
with TVTForward.Main.Skel;

package body TVTForward.Main.Impl is


   function sendSecond
     (Self : access Object;
      f : in TVTForward.first.Value_Ref)
     return TVTForward.second.Value_Ref
   is
      Result : TVTForward.second.Value_Ref;
   begin
      First.Print (F);
      Result := First.Get_Att (F);
      return Result;
   end sendSecond;

end TVTForward.Main.Impl;
