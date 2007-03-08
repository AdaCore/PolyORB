with Types; use Types;

package Locations is

   type Location is record
      File  : Name_Id;
      Dir   : Name_Id;
      Line  : Int;
      First : Text_Ptr;
      Last  : Text_Ptr;
      Scan  : Text_Ptr;
   end record;

   No_Location : constant Location := (No_Name, No_Name, 0, 0, 0, 0);

   function Image (Loc : Location) return String;

   procedure Set_New_Location
     (Loc  : in out Location;
      Name : Name_Id;
      Line : Int);

   function "<" (Op1, Op2 : Location) return Boolean;

end Locations;
