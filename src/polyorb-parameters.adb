------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . P A R A M E T E R S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  PolyORB runtime configuration facility

with PolyORB.Log;
with PolyORB.Utils;

package body PolyORB.Parameters is

   function To_Boolean (V : String) return Boolean;
   --  Convert a String value to a Boolean value according
   --  to the rules indicated in the spec for boolean configuration
   --  variables.

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

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf
     (Section, Key : String;
      Default      : String := "")
     return String
   is
   begin
      if Get_Conf_Hook /= null then
         return Get_Conf_Hook.all (Section, Key, Default);

      else
         return Default;
      end if;
   end Get_Conf;

   function Get_Conf
     (Section, Key : String;
      Default      : Boolean := False)
     return Boolean
   is
      Default_Value : constant array (Boolean'Range) of
        String (1 .. 1) := (False => "0", True => "1");
   begin
      return To_Boolean (Get_Conf (Section, Key, Default_Value (Default)));
   end Get_Conf;

   function Get_Conf
     (Section, Key : String;
      Default      : Integer := 0)
     return Integer
   is
   begin
      return Integer'Value (Get_Conf (Section, Key, Integer'Image (Default)));
   end Get_Conf;

   -------------
   -- Get_Env --
   -------------

   function Get_Env
     (Key     : String;
      Default : String := "")
     return String
   is
   begin
      if Fetch_From_Env_Hook /= null then
         return Fetch_From_Env_Hook.all (Key, Default);
      else
         return Default;
      end if;
   end Get_Env;

   --------------
   -- Set_Conf --
   --------------

   procedure Set_Conf
     (Section, Key : String;
      Value        : String)
   is
   begin
      if Set_Conf_Hook /= null then
         Set_Conf_Hook.all (Section, Key, Value);

      else
         raise Program_Error;
      end if;
   end Set_Conf;

   ----------------
   -- Initialize --
   ----------------

   procedure Set_Hooks is
   begin
      PolyORB.Log.Get_Conf_Hook := Get_Conf'Access;
   end Set_Hooks;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      if Reset_Hook /= null then
         Reset_Hook.all;

      else
         raise Program_Error;
      end if;
   end Reset;

end PolyORB.Parameters;
