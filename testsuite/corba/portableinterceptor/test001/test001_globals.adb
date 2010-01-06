------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 1 _ G L O B A L S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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
