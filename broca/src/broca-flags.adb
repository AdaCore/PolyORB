with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Calendar;

package body Broca.Flags is
   --  Scan all options from the command line and interprete corba options.
   --  CORBA options begins with "-ORB" and can have an optional argument.
   --  CORBA options are not removed from the command line, which can be
   --  a problem for the user program, eg considere this one:
   --  ./my_server -ORBopt foo bar
   --  Is `foo' the argument of `-ORBopt' or the first argument of the program.
   --  FIXME.
   procedure Decode_Options;

   --  Set the value of boot_time.
   procedure Set_Boot_Time;

   procedure Set_Boot_Time
   is
      use Ada.Calendar;
      C : Time;
   begin
      C := Clock;
      --  Upper bound value of Initial_Time is 4294967295.
      --  upper bound values of second is 86400
      --                        day       31
      --                        month     12
      --  This is enough for 128 years.
      Boot_Time := CORBA.Unsigned_Long
        (Seconds (C) +
         (Day_Duration'Last * (Day (C) +
                               (Day_Number'Last * (Month (C) +
                                                   Month_Number'Last *
                                                   (Year (C) mod 128))))));
   end Set_Boot_Time;

   procedure Decode_Options
   is
      use Ada.Command_Line;
      Index : Positive;
   begin
      Index := 1;
      while Index <= Argument_Count loop
         if Argument (Index) = "-ORBport" then
            Index := Index + 1;
            Port := Natural'Value (Argument (Index));
         elsif Argument (Index) = "-ORBserver-tasks" then
            Index := Index + 1;
            Nbr_Server_Tasks := Natural'Value (Argument (Index));
         elsif Argument (Index) = "-ORBlog" then
            Broca.Flags.Log := True;
         elsif Argument (Index)'Length >= 4
           and then Argument (Index)(1 .. 4) = "-ORB"
         then
            Ada.Text_IO.Put_Line
              (Command_Name & ": option `" & Argument (Index)
               & "' is unknown and ignored");
         end if;
         Index := Index + 1;
      end loop;
   end Decode_Options;
begin
   Set_Boot_Time;
   Decode_Options;
end Broca.Flags;
