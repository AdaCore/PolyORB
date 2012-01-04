------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S T R I N G S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  General-purpose string pointer.

package body PolyORB.Utils.Strings is

   ---------
   -- "+" --
   ---------

   function "+" (S : Standard.String) return String_Ptr is
   begin
      return new Standard.String'(S);
   end "+";

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (V : String) return Boolean is
      VV : constant String := PolyORB.Utils.To_Lower (V);

   begin
      if VV'Length > 0 then
         case VV (VV'First) is
            when '0' | 'n' =>
               return False;

            when '1' | 'y' =>
               return True;

            when 'o' =>
               if VV = "off" then
                  return False;

               elsif VV = "on" then
                  return True;
               end if;

            when 'd' =>
               if VV = "disable" then
                  return False;
               end if;

            when 'e' =>
               if VV = "enable" then
                  return True;
               end if;

            when 'f' =>
               if VV = "false" then
                  return False;
               end if;

            when 't' =>
               if VV = "true" then
                  return True;
               end if;

            when others =>
               null;
         end case;
      end if;

      raise Constraint_Error;
   end To_Boolean;

end PolyORB.Utils.Strings;
