
--  This package provides types used by all MOMA packages, it is mostly derived
--  from PolyORB.Types

with PolyORB.Types;

package MOMA.Types is

   --
   --  Generic types
   --

   subtype String is PolyORB.Types.String;

   function To_Standard_String (V : PolyORB.Types.String)
                                return Standard.String
     renames PolyORB.Types.To_Standard_String;

   function To_MOMA_String (V : Standard.String)
                            return PolyORB.Types.String
     renames PolyORB.Types.To_PolyORB_String;

   --
   --  MOMA specific types
   --

   type Meta_Data is new Integer;
   type Acknowledge_Type is new Integer;
   type Property_Type is new Integer;
   type String_Ptr is access String;
   type Priority is new Integer range 1 .. 10;
   type Record_Type is new Integer;
   type Array_Type is new Integer;

end MOMA.Types;

