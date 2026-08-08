with Interfaces; use Interfaces;

package body Perfect_Hash is

   P : constant array (0 .. 3) of Natural :=
     (2, 4, 10, 13);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (28, 62, 54, 61);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (20, 37, 10, 27);

   G : constant array (0 .. 94) of Unsigned_8 :=
     (38, 33, 12, 0, 0, 0, 0, 0, 10, 0, 35, 0, 10, 0, 18, 3, 0, 9, 0, 2, 0,
      0, 23, 0, 0, 0, 0, 41, 13, 0, 21, 0, 3, 0, 23, 34, 8, 0, 0, 0, 28, 0,
      32, 0, 42, 0, 43, 0, 0, 31, 22, 0, 34, 0, 0, 19, 0, 19, 0, 13, 0, 0,
      0, 0, 0, 5, 0, 41, 0, 6, 14, 0, 2, 0, 33, 1, 23, 23, 3, 0, 0, 0, 0, 0,
      5, 41, 11, 0, 12, 0, 19, 0, 36, 6, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 95;
         F2 := (F2 + Natural (T2 (K)) * J) mod 95;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 47;
   end Hash;

end Perfect_Hash;
