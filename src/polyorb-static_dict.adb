--  A static dictionnary of objects, indexed by values of
--  an enumerated type.

--  $Id$

package body PolyORB.Static_Dict is

   function New_Dict return Dict_Access is
   begin
      return new Dict;
   end New_Dict;

   function Lookup
     (D : Dict;
      K : Key)
     return Value is
   begin
      return D (K);
   end Lookup;

   procedure Register
     (D : in out Dict;
      K : Key;
      V : Value) is
   begin
      D (K) := V;
   end Register;

end PolyORB.Static_Dict;

