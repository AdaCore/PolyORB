--  A static dictionnary of objects, indexed by values of
--  an enumerated type.

--  $Id$

generic

   type Key is (<>);
   type Value is private;

package PolyORB.Static_Dict is

   pragma Preelaborate;

   type Dict is private;
   type Dict_Access is access all Dict;

   function New_Dict return Dict_Access;

   function Lookup
     (D : Dict;
      K : Key)
     return Value;

   procedure Register
     (D : in out Dict;
      K :        Key;
      V :        Value);

private

   type Dict is array (Key'Range) of Value;

end PolyORB.Static_Dict;

