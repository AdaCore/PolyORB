--  Simple test case for Droopi.Soft_Links.Task_Attributes.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with Droopi.Soft_Links;
with Droopi.No_Tasking;

package body  Droopi.Test.Task_Attributes is

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

end Droopi.Test.Task_Attributes;
