------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . U T I L S . S I M P L E _ F L A G S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.Utils.Simple_Flags is

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.utils.simple_flags");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Flag_To_Test : Flags;
                    In_Flags     : Flags) return Boolean
   is
      Result : Boolean;
      Temp_Flags : Flags := In_Flags;
      Counter : Flags := 1;
   begin

      while Counter /= Flag_To_Test loop
         Temp_Flags := Temp_Flags / 2;
         Counter := 2 * Counter;
      end loop;

      Result := not (Temp_Flags mod 2 = 0);

      pragma Debug (O ("Is_Set " & Integer'Image (Integer (Flag_To_Test))
                       & " in " & Integer'Image (Integer (In_Flags))
                       & " : " & Boolean'Image (Result)));

      return Result;
   end Is_Set;

   ---------
   -- Set --
   ---------

   function Set (Flag_To_Set : Flags;
                 In_Flags    : Flags) return Flags
   is
   begin
      if not Is_Set (Flag_To_Set, In_Flags) then
         return In_Flags + Flag_To_Set;
      else
         return In_Flags;
      end if;
   end Set;

end PolyORB.Utils.Simple_Flags;
