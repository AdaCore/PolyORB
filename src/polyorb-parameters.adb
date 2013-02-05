------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . P A R A M E T E R S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
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

--  PolyORB runtime configuration facility

pragma Ada_2005;

with PolyORB.Constants;
with PolyORB.Initialization;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Parameters is

   package Source_Lists is
     new PolyORB.Utils.Chained_Lists (Parameters_Source_Access);
   type Source_List_Access is access Source_Lists.List;

   Sources : Source_List_Access;
   --  Manage an ordered list of configuration parameter sources. When looking
   --  up the value of a parameter, the first match is returned, hence sources
   --  closer to the head of the list take precendence over subsequent ones.
   --  The designated List object is allocated on the first call to
   --  Register_Source (we can't declare a List object directly here because
   --  List is a private type, and this would make Parameters non-preelaborable
   --  in Ada 95).

   function Fetch (Key : String) return String;
   --  Get the string from a file (if Key starts with file: and the file
   --  exists, otherwise it is an empty string), or the string itself
   --  otherwise. Also expands macros in the returned value if the
   --  Expand_Macros hook is not null.

   -----------
   -- Fetch --
   -----------

   function Fetch (Key : String) return String is

      function Fetch_From_File return String;
      --  Helper function to fetch the value from a file, if necessary

      ---------------------
      -- Fetch_From_File --
      ---------------------

      function Fetch_From_File return String is
      begin
         if PolyORB.Utils.Has_Prefix (Key, "file:")
           and then Fetch_From_File_Hook /= null
         then
            return Fetch_From_File_Hook (Key);
         else
            return Key;
         end if;
      end Fetch_From_File;

      Val : constant String := Fetch_From_File;

   --  Start of processing for Fetch

   begin
      if Expand_Macros_Hook /= null then
         return Expand_Macros_Hook (Val);
      else
         return Val;
      end if;
   end Fetch;

   -----------------------
   -- Get_Conf (String) --
   -----------------------

   function Get_Conf
     (Section, Key : String;
      Default      : String := "") return String
   is
      use Source_Lists;
      It : Iterator;
   begin
      if Sources = null then
         return Default;
      end if;

      It := First (Sources.all);

      while not Last (It) loop
         declare
            V : constant String := Get_Conf (Value (It).all, Section, Key);
         begin
            if V'Length > 0 then
               return Fetch (V);
            end if;
         end;
         Next (It);
      end loop;
      return Default;
   end Get_Conf;

   ------------------------
   -- Get_Conf (Boolean) --
   ------------------------

   function Get_Conf
     (Section, Key : String;
      Default      : Boolean := False) return Boolean
   is
      Default_Value : constant array (Boolean'Range) of
        String (1 .. 1) := (False => "0", True => "1");
   begin
      return Utils.Strings.To_Boolean
        (Get_Conf (Section, Key, Default_Value (Default)));
   end Get_Conf;

   -------------------------
   -- Get_Conf (Duration) --
   -------------------------

   function Get_Conf
     (Section, Key : String;
      Default      : Duration := 0.0) return Duration
   is
      Default_Milliseconds : Integer;
      Milliseconds         : Integer;
   begin
      if Default = Constants.Forever then
         Default_Milliseconds := -1;
      else
         Default_Milliseconds := Natural (Default * 1000);
      end if;
      Milliseconds := Get_Conf (Section, Key, Default_Milliseconds);
      if Milliseconds < 0 then
         return Constants.Forever;
      else
         return Milliseconds * 0.001;
      end if;
   end Get_Conf;

   ------------------------
   -- Get_Conf (Integer) --
   ------------------------

   function Get_Conf
     (Section, Key : String;
      Default      : Integer := 0) return Integer
   is
   begin
      return Integer'Value (Get_Conf (Section, Key, Integer'Image (Default)));
   end Get_Conf;

   -------------------------
   -- Get_Conf (Interval) --
   -------------------------

   function Get_Conf
     (Section, Key : String;
      Default      : Interval := (0, 0)) return Interval
   is
      Default_Str : constant String := Default.Lo'Img & "-" & Default.Hi'Img;
   begin
      return Utils.To_Interval (Get_Conf (Section, Key, Default_Str));
   end Get_Conf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.Initialization.Get_Conf_Hook := Get_Conf'Access;
   end Initialize;

   ---------------------
   -- Make_Global_Key --
   ---------------------

   function Make_Global_Key (Section, Key : String) return String is
   begin
      return "[" & Section & "]" & Key;
   end Make_Global_Key;

   ---------------------
   -- Register_Source --
   ---------------------

   procedure Register_Source (Source : Parameters_Source_Access) is
   begin
      if Sources = null then
         Sources := new Source_Lists.List;
      end if;
      Source_Lists.Append (Sources.all, Source);
   end Register_Source;

end PolyORB.Parameters;
