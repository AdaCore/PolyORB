------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

package body Prj.Ext is

   type Node;
   type Link is access Node;
   type Node is record
     Name  : String_Access;
     Value : String_Access;
     Next  : Link;
   end record;

   First : Link;
   --  The external reference cache.

   ---------
   -- Add --
   ---------

   procedure Add
     (External_Name : String;
      Value         : String)
   is
      Current : Link;

   begin
      --  First, check the cache.
      --  If there is already an entry, replace the value.

      Current := First;
      while Current /= null loop
         if Current.Name.all = External_Name then
            Free (Current.Value);
            Current.Value := new String'(Value);
            return;

         else
            Current := Current.Next;
         end if;
      end loop;

      --  It is not in the cache, add a new entry.

      First := new Node'(Name => new String'(External_Name),
                         Value => new String'(Value),
                         Next => First);
   end Add;

   -----------
   -- Check --
   -----------

   function Check (Declaration : String) return Boolean is
      Equal_Pos : constant Natural :=
                    Index (Source => Declaration, Pattern => "=");

   begin
      if Equal_Pos /= 0 and then Equal_Pos /= Declaration'First then
         Add
           (External_Name => Declaration (Declaration'First .. Equal_Pos - 1),
            Value         => Declaration (Equal_Pos + 1 .. Declaration'Last));
         return True;

      else
         return False;
      end if;
   end Check;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (External_Name : String;
      With_Default  : String := "")
      return          String
   is
      Current : Link;

   begin
      --  First look in the cache

      Current := First;
      while Current /= null loop
         if Current.Name.all = External_Name then
            if Current.Value = null then
               return With_Default;
            else
               return Current.Value.all;
            end if;

         else
            Current := Current.Next;
         end if;
      end loop;

      --  It is not in the cache, add an entry

      First := new Node'(Name => new String'(External_Name),
                         Value => null,
                         Next => First);

      --  Find if it is an environment.
      --  If it is, put the value in the new cache entry.

      declare
         Env_Value : constant String_Access := Getenv (External_Name);

      begin
         if Env_Value /= null and then
            Env_Value'Length > 0 then
            First.Value := new String'(Env_Value.all);
            return Env_Value.all;

         else
            return With_Default;
         end if;
      end;
   end Value_Of;

end Prj.Ext;
