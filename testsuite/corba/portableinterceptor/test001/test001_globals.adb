------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 1 _ G L O B A L S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

with Ada.Characters.Handling;

with PolyORB.Utils.Report;

package body Test001_Globals is

   -----------
   -- Image --
   -----------

   function Image (Value : Interception_Point) return String is
   begin
      return
        Ada.Characters.Handling.To_Lower (Interception_Point'Image (Value));
   end Image;

   ------------
   -- Output --
   ------------

   procedure Output
     (Point     : Interception_Point;
      Operation : String;
      Status    : Boolean;
      Comment   : String := "")
   is
   begin
      if Point in Client_Interception_Point then
         PolyORB.Utils.Report.Output
           ("[" & Image (Point) & "] CRI::" & Operation & Comment, Status);
      else
         PolyORB.Utils.Report.Output
           ("[" & Image (Point) & "] SRI::" & Operation & Comment, Status);
      end if;
   end Output;

end Test001_Globals;
