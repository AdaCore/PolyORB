--  $Id$

package body PolyORB.Dict is

   use Dict_Entry_Seqs;

   procedure Set
     (D : in out Dict; K : Key; V : Value; Index : out Natural)
   is
      Existing : constant Natural := Find_Key (D, K);
   begin
      if Existing /= Null_Index then
         raise Duplicate_Key;
      end if;

      Append (D.Entries, Dict_Entry'(K, V));
      Index := Length (D.Entries);
   end Set;

   function Find_Key (D : Dict; K : Key) return Natural
   is
      Elements : constant Element_Array
        := To_Element_Array (D.Entries);
   begin
      for I in Elements'Range loop
         if Elements (I).K = K then
            return 1 + I - Elements'First;
         end if;
      end loop;

      return Null_Index;
   end Find_Key;

   function Find_Value (D : Dict; V : Value) return Natural
   is
      Elements : constant Element_Array
        := To_Element_Array (D.Entries);
   begin
      for I in Elements'Range loop
         if Elements (I).V = V then
            return 1 + I - Elements'First;
         end if;
      end loop;

      return Null_Index;
   end Find_Value;

   function Get (D : Dict; K : Key) return Value
   is
      Index : constant Natural := Find_Key (D, K);
   begin
      if Index = Null_Index then
         raise Bad_Element;
      end if;

      return Get_By_Index (D, Index);
   end Get;

   function Get_By_Index (D : Dict; Index : Natural) return Value is
   begin
      return Element_Of (D.Entries, Index).V;
   end Get_By_Index;

   procedure Remove (D : in out Dict; K : Key; V : out Value)
   is
      Index : constant Natural := Find_Key (D, K);
   begin
      if Index = Null_Index then
         raise Bad_Element;
      end if;

      Remove_By_Index (D, Index, V);
   end Remove;

   procedure Remove_By_Index
     (D : in out Dict; Index : Natural; V : out Value) is
   begin
      V := Get_By_Index (D, Index);
      Delete (D.Entries, Index, Index);
   end Remove_By_Index;

end PolyORB.Dict;
