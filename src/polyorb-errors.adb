------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . E R R O R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Log;

package body PolyORB.Errors is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.errors");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

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
         pragma Debug (C, O ("*** Abort *** "
                          & Error_Id'Image (Error.Kind)));

         Free (Error.Member);
      end if;

      Error.Kind := Kind;
      Error.Member := new Exception_Members'Class'(Member);

      pragma Debug (C, O ("*** Throw *** " & Error_Id'Image (Error.Kind)));
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
