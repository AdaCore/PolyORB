------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             D Y N _ D I C T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Real_Time;

with Utils;
with PolyORB.Dynamic_Dict;
with PolyORB.Utils.Report;
with PolyORB.Utils.Strings;

package body Dyn_Dict is
   use Ada.Real_Time;
   use Ada.Text_IO;

   use Utils;
   use Utils.Duration_Stats;

   use PolyORB.Utils.Report;
   use PolyORB.Utils.Strings;

   package My_Dict is new PolyORB.Dynamic_Dict (Value => String_Ptr);

   -------------------
   -- Test_Register --
   -------------------

   procedure Test_Register
     (Stamp : Standard.String;
      How_Many : Natural)
   is
      Key_Root   : constant String := "Key";
      Value_Root : constant String := "Root";

      T0, T1, T2 : Ada.Real_Time.Time;
      Results : Stat_Vector (1 .. How_Many);

   begin
      New_Test ("Perfact Dynamic Hash Table");
      Put_Line ("Description: test O (n), amortized O (1) register time, "
                & "O (1) lookup time");

      for J in 1 .. How_Many loop
         declare
            Count : constant String := Natural'Image (J);
            Key   : constant String := Key_Root
              & Count (Count'First + 1 .. Count'Last);
            Value : constant String := Value_Root
              & Count (Count'First + 1 .. Count'Last);

         begin
            T0 := Clock;
            My_Dict.Register (Key, +Value);
            T1 := Clock;
            T2 := Clock;
            Results (J) := To_Duration (T1 - T0 - (T2 - T1));
         end;

      end loop;
      Analyse_Vector (Results, Stamp & "dyn_dict_register");

      for J in 1 .. How_Many loop
         declare
            Count : constant String := Natural'Image (J);
            Key   : constant String := Key_Root
              & Count (Count'First + 1 .. Count'Last);
            Value : constant String := Value_Root
              & Count (Count'First + 1 .. Count'Last);
            Content : String_Ptr;
         begin
            T0 := Clock;
            Content := My_Dict.Lookup (Key, Default => null);
            T1 := Clock;
            T2 := Clock;
            Results (J) := To_Duration (T1 - T0 - (T2 - T1));

            if Content = null or else Value /= Content.all then
                  Output ("Regression occured for key "
                          & Key
                          & " at stage #"
                          & Integer'Image (How_Many),
                          False);
                  raise Program_Error;
            end if;
         end;
      end loop;
      Analyse_Vector (Results, Stamp & "dyn_dict_lookup");

      Output ("Test completed", True);
   end Test_Register;

end Dyn_Dict;
