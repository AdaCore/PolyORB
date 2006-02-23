with GNAT.OS_Lib; use GNAT.OS_Lib;

with Namet; use Namet;

package body Locations is

   ---------
   -- "<" --
   ---------

   function "<" (Op1, Op2 : Location) return Boolean is
   begin
      return Op1.File = Op2.File
        and then Op1.Dir = Op2.Dir
        and then Op1.Scan < Op2.Scan;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (Loc : in Location) return String
   is
      Column : constant Nat := Nat (Loc.Last - Loc.First + 1);
   begin
      if Loc.File = No_Name then
         return No_Str;
      end if;
      Get_Name_String (Loc.File);
      Add_Char_To_Name_Buffer (':');
      Add_Nat_To_Name_Buffer (Nat (Loc.Line));
      Add_Char_To_Name_Buffer (':');
      if Column < 10 then
         Add_Char_To_Name_Buffer ('0');
      end if;
      Add_Nat_To_Name_Buffer (Column);
      return Name_Buffer (1 .. Name_Len);
   end Image;

   ----------------------
   -- Set_New_Location --
   ----------------------

   procedure Set_New_Location
     (Loc  : in out Location;
      Name : Name_Id;
      Line : Int)
   is
      Len : Integer;
   begin
      Loc.Line := Line;
      Get_Name_String (Name);
      Len := Name_Len;
      for I in reverse 1 .. Name_Len loop
         if Name_Buffer (I) = Directory_Separator then
            Name_Len := I - 1;
            Loc.Dir  := Name_Find;
            Set_Str_To_Name_Buffer (Name_Buffer (I + 1 .. Len));
            Loc.File := Name_Find;
            return;
         end if;
      end loop;
      Loc.Dir  := No_Name;
      Loc.File := Name;
   end Set_New_Location;

end Locations;
