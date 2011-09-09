------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . U T I L S . B A C K T R A C E               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2011, Free Software Foundation, Inc.             --
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

with Ada.Strings.Unbounded;
with System.Address_Image;
with GNAT.Traceback;

package body PolyORB.Utils.Backtrace is

   use Ada.Strings.Unbounded;
   use GNAT.Traceback;

   ---------------
   -- Backtrace --
   ---------------

   function Backtrace return String is
      Tra : Tracebacks_Array (1 .. 64);
      Len : Natural;
      Res : Unbounded_String;
   begin
      Call_Chain (Tra, Len);
      for J in Tra'First .. Len loop
         if Length (Res) > 0 then
            Append (Res, ' ');
         end if;
         Append (Res, System.Address_Image (Tra (J)));
      end loop;
      return To_String (Res);
   end Backtrace;

end PolyORB.Utils.Backtrace;
