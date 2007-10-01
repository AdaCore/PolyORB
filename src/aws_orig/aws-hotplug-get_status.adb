------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               A W S . H O T P L U G . G E T _ S T A T U S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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
