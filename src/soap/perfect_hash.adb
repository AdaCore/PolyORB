with Interfaces; use Interfaces;

package body Perfect_Hash is

   P : constant array (0 .. 1) of Natural :=
     (1, 2);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (10, 3);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (1, 9);

   G : constant array (0 .. 15) of Unsigned_8 :=
     (0, 6, 0, 0, 7, 2, 0, 7, 0, 0, 0, 7, 0, 4, 6, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for I in P'Range loop
         exit when L < P (I);
         J  := Character'Pos (S (P (I) + F));
         F1 := (F1 + Natural (T1 (I)) * J) mod 16;
         F2 := (F2 + Natural (T2 (I)) * J) mod 16;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 8;
   end Hash;

end Perfect_Hash;
