------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R E P O R T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/polyorb-utils-report.adb#2 $

with Ada.Text_IO;

package body PolyORB.Utils.Report is

   Max : constant Natural := 60;
   Passed : Boolean := True;

   ------------
   -- Output --
   ------------

   procedure Output
     (Message : in String;
      Result  : in Boolean)
   is
      Line : String (1 .. Max) := (others => '.');
      Last : Natural := Message'Length;
   begin
      if Last > Max then
         Last := Max;
      end if;

      Line (1 .. Last) := Message (Message'First .. Message'First + Last - 1);

      if Result then
         Ada.Text_IO.Put_Line (Line & ": PASSED");
      else
         Ada.Text_IO.Put_Line (Line & ": FAILED");
      end if;

      Passed := Passed and then Result;
   end Output;

   ----------------
   -- End_Report --
   ----------------

   procedure End_Report is
   begin
      Output ("END TESTS", Passed);
   end End_Report;

   --------------
   -- New_Test --
   --------------

   procedure New_Test (Test_Name : String) is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("==> Begin test " & Test_Name & " <==");
   end New_Test;

end PolyORB.Utils.Report;

