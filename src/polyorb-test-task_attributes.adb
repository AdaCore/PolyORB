------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T E S T . T A S K _ A T T R I B U T E S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Simple test case for PolyORB.Soft_Links.Task_Attributes.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Soft_Links;
with PolyORB.No_Tasking;

package body  PolyORB.Test.Task_Attributes is

   package Attr1 is new Soft_Links.Task_Attributes
     (Integer, 123);
   package Attr2 is new Soft_Links.Task_Attributes
     (Character, 'A');

   P : Attr2.Attribute_Handle;

begin
   No_Tasking.Initialize;

   Put_Line ("@@1");
   Attr1.Set_Value (456);
   Put_Line ("@@2");

   Put_Line ("Attr1: 456 =" & Attr1.Value'Img);






   Put_Line ("Attr2: A = " & Attr2.Value'Img);

   Attr1.Reinitialize;
   P := Attr2.Reference;

   Put_Line ("Attr1: 123 =" & Attr1.Value'Img);
   Put_Line ("Attr2: A = " & Attr2.Value'Img);

   Attr1.Set_Value (789);
   P.all := 'B';

   Put_Line ("Attr1: 789 =" & Attr1.Value'Img);
   Put_Line ("Attr2: B = " & Attr2.Value'Img);

end PolyORB.Test.Task_Attributes;
