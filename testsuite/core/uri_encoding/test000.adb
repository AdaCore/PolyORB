------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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
