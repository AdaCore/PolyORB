--  User Defined Libraries
with Common; use Common;

package Prime is

   pragma Remote_Types;

   type New_Pool_Type is new Prime_Pool_Type with null record;

   procedure Test_Primarity
     (Pool    : access New_Pool_Type;
      Number  : in     Natural);

   --  pragma Asynchronous (Test_Primarity) is expected here
   --  but when applied to RACW, all subprograms with only
   --  'in' parameters are automatically declared as asynchronous .

end Prime;
