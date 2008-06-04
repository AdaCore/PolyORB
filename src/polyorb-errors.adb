------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . E R R O R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Log;

package body PolyORB.Errors is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.errors");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -----------
   -- Found --
   -----------

   function Found (Error : Error_Container) return Boolean is
   begin
      return Error.Kind /= No_Error;
   end Found;

   -----------
   -- Throw --
   -----------

   procedure Throw
     (Error  : in out Error_Container;
      Kind   : Error_Id;
      Member : Exception_Members'Class)
   is
   begin
      if Error.Kind /= No_Error then
         pragma Debug (O ("*** Abort *** "
                          & Error_Id'Image (Error.Kind)));

         Free (Error.Member);
      end if;

      Error.Kind := Kind;
      Error.Member := new Exception_Members'Class'(Member);

      pragma Debug (O ("*** Throw *** " & Error_Id'Image (Error.Kind)));
   end Throw;

   -----------
   -- Catch --
   -----------

   procedure Catch
     (Error : in out Error_Container) is
   begin
      Error.Kind := No_Error;
      Free (Error.Member);
   end Catch;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Error : Error_Container) return Boolean is
   begin
      return Error.Kind /= No_Error;
   end Is_Error;

end PolyORB.Errors;
