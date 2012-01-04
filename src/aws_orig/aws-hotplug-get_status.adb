------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               A W S . H O T P L U G . G E T _ S T A T U S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

function AWS.Hotplug.Get_Status
  (Filters : Filter_Set) return Templates_Parser.Translate_Table
is

   use Templates_Parser;

   Regexp : Vector_Tag;
   URL    : Vector_Tag;

   --  Avoid : may be referenced before it has a value
   pragma Warnings (Off, Regexp);
   pragma Warnings (Off, URL);

begin
   for K in 1 .. Filters.Count loop
      Regexp := Regexp & Filters.Set (K).Regexp_Str;
      URL    := URL    & Filters.Set (K).URL;
   end loop;

   return Translate_Table'(Assoc ("HP_REGEXP_V", Regexp),
                           Assoc ("HP_URL_V",    URL));
end AWS.Hotplug.Get_Status;
