--
--  $Id$
--

package Test_RAS_Remote is

   pragma Remote_Call_Interface;

   type Arithmetic_Operation is
      access function (X, Y : Integer) return Integer;

   function Perform (A, B : Integer;
                     Op   : Arithmetic_Operation)
     return Integer;

   function Add (X, Y : Integer) return Integer;
   function Multiply (X, Y : Integer) return Integer;

end Test_RAS_Remote;
