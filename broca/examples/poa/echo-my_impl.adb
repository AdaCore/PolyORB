with Ada.Text_IO; use Ada.Text_IO;
with Server;
with Echo.Stream;

package body Echo.My_Impl is
   function EchoString (Self : access Object; Mesg : in CORBA.String)
                        return CORBA.String is
   begin
      if not Server.Flag_Wall then
         Echo.Stream.Raise_no_walls ((One => 123));
      end if;

      if Self.Delay_Time > 0.0 then
         Put_Line ("echostring: delaying");
         delay Self.Delay_Time;
      end if;
      return Mesg;
   end EchoString;
end Echo.My_Impl;

