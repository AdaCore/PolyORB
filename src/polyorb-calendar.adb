------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . C A L E N D A R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

with PolyORB.Log;

package body PolyORB.Calendar is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.calendar");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   The_Clock_Factory : Clock_Factory_Access := null;

   ------------
   -- Create --
   ------------

   function Create return Time_Type_Access
   is
   begin
      pragma Assert (The_Clock_Factory /= null);
      return Create (The_Clock_Factory);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Clock : in out Time_Type_Access)
   is
   begin
      pragma Assert (The_Clock_Factory /= null);
      Destroy (The_Clock_Factory, Clock);
   end Destroy;

   -----------
   -- Clock --
   -----------

   function Clock return Time_Type'Class
   is
   begin
      pragma Assert (The_Clock_Factory /= null);
      return Clock (The_Clock_Factory);
   end Clock;

   ----------------------------
   -- Register_Clock_Factory --
   ----------------------------

   procedure Register_Clock_Factory (CF : Clock_Factory_Access)
   is
   begin
      pragma Assert (The_Clock_Factory = null);
      pragma Debug (O ("register clock factory"));
      The_Clock_Factory := CF;
   end Register_Clock_Factory;

end PolyORB.Calendar;
