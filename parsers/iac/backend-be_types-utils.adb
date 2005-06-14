with Output; use Output;

package body Backend.BE_Types.Utils is

   --  Local declarations

   function Is_Member (L : List; T : TCKind) return Boolean;
   --  Return False if the TCKind is not an element of L.

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member (L : List; T : TCKind) return Boolean is
      I : Iterator := First (L);
   begin
      while not Last (I) loop
         if I.The_Cell.Typ = T then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Is_Member;

   -----------
   -- First --
   -----------

   function First (L : List) return Iterator is
   begin
      return Iterator'(The_Cell => L.First);
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return Iterator is
      pragma Unreferenced (L);
   begin
      return Iterator'(The_Cell => null);
   end Last;

   function Last (I : Iterator) return Boolean is
   begin
      return I.The_Cell = null;
   end Last;

   ----------
   -- Next --
   ----------

   procedure Next (I : in out Iterator) is
   begin
      I.The_Cell  := I.The_Cell.Next;
   end Next;

   ------------
   -- Insert --
   ------------

   procedure Insert (L : in out List; T : TCKind) is
      C : Cell_Access;
   begin
      if not Is_Member (L, T) then
         C := new Cell'(Typ => T, Next => L.First);
         L.First := C;
      end if;
   end Insert;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (L : List) is
      I : Iterator := First (L);
   begin
      while not Last (I) loop
         Write_Line (TCKind'Image (I.The_Cell.Typ));
         Next (I);
      end loop;
   end Print_List;

end Backend.BE_Types.Utils;
