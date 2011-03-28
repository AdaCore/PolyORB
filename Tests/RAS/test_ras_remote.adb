--
--  $Id$
--

package body Test_RAS_Remote is

   ---------
   -- Add --
   ---------

   function Add (X, Y : Integer) return Integer is
   begin
      return X + Y;
   end Add;

   --------------
   -- Multiply --
   --------------

   function Multiply (X, Y : Integer) return Integer is
   begin
      return X * Y;
   end Multiply;

   -------------
   -- Perform --
   -------------

   function Perform (A, B : Integer;
                     Op   : Arithmetic_Operation)
     return Integer
   is
   begin
      return Op (A, B);
   end Perform;

end Test_RAS_Remote;
