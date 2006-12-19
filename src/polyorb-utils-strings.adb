------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S T R I N G S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2001 Free Software Foundation, Inc.             --
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

--  General-purpose string pointer.

package body PolyORB.Utils.Strings is

   ---------
   -- "+" --
   ---------

   function "+"
     (S : Standard.String)
     return String_Ptr is
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
