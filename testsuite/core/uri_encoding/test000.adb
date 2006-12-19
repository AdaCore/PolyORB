------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with PolyORB.Utils.Report;

procedure Test000 is

   use PolyORB.Utils;
   use PolyORB.Utils.Report;

begin
   New_Test ("URI encoding & decoding");

   --  The following tests are detailled in CORBA Naming Service
   --  Specification, v1.3, par 2.5.3.5.

   Output ("a.b/c.d -> " & URI_Encode ("a.b/c.d", Also_Escape => " "),
           URI_Encode ("a.b/c.d", Also_Escape => " ") = "a.b/c.d");

   Output ("<a>.b/c.d -> " & URI_Encode ("<a>.b/c.d", Also_Escape => " "),
           URI_Encode ("<a>.b/c.d", Also_Escape => " ") = "%3ca%3e.b/c.d");

   Output ("a.b/  c.d -> " & URI_Encode ("a.b/  c.d", Also_Escape => " "),
           URI_Encode ("a.b/  c.d", Also_Escape => " ") = "a.b/%20%20c.d");

   Output ("a%b/c%d -> " & URI_Encode ("a%b/c%d", Also_Escape => " "),
           URI_Encode ("a%b/c%d", Also_Escape => " ") = "a%25b/c%25d");

   Output ("a\\b/c.d -> " & URI_Encode ("a\\b/c.d", Also_Escape => " "),
           URI_Encode ("a\\b/c.d", Also_Escape => " ") = "a%5c%5cb/c.d");

   Output ("a.b/c.d -> " & URI_Decode ("a.b/c.d"),
           URI_Decode ("a.b/c.d") = "a.b/c.d");

   Output ("%3ca%3e.b/c.d -> " & URI_Decode ("%3ca%3e.b/c.d"),
           URI_Decode ("%3ca%3e.b/c.d") = "<a>.b/c.d");

   Output ("a.b/%20%20c.d -> " & URI_Decode ("a.b/%20%20c.d"),
           URI_Decode ("a.b/%20%20c.d") = "a.b/  c.d");

   Output ("a%25b/c%25d -> " & URI_Decode ("a%25b/c%25d"),
           URI_Decode ("a%25b/c%25d") = "a%b/c%d");

   Output ("a%5c%5cb/c.d -> " & URI_Decode ("a%5c%5cb/c.d"),
           URI_Decode ("a%5c%5cb/c.d") = "a\\b/c.d");

   End_Report;
end Test000;
