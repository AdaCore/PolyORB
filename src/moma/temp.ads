--  This package does just exist in order to provide a few variables needed
--  in order to compile the specs. The types provided for the different
--  variables aren't relevant at all.

package Temp is

   type Meta_Data is new Integer;
   type Acknowledge_Type is new Integer;
   type Property_Type is new Integer;
   type String_Ptr is access String;
   type Priority is new Integer range 1 .. 10;
   type Record_Type is new Integer;
   type Array_Type is new Integer;

end Temp;
