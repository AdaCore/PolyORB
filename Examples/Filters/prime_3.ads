--  User Defined Libraries
with Common; use Common;

package Prime_3 is
   pragma Remote_Call_Interface;

   procedure Test_Primarity
     (Number  : in  Natural;
      Divider : out Natural;
      Where   : out Partition_ID);

end Prime_3;
