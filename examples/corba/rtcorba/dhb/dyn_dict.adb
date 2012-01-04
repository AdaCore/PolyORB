------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             D Y N _ D I C T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
