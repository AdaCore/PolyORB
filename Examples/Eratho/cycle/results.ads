--  User Defined Libraries
with Common; use Common;

package Results is
   pragma Remote_Call_Interface;

   procedure Save
     (Divider : in Natural;
      Where   : in Partition_ID);
   pragma Asynchronous (Save);

   procedure Load_When_Ready
     (Divider : out Natural;
      Where   : out Partition_ID);

end Results;

