with Common; use Common;

package Controller is

   pragma Remote_Call_Interface;

   type Pool_Access is access all Pool_Type'Class;
   pragma Asynchronous (Pool_Access);

   --  Note that pragma Asynchronous is valid only in a RCI package.
   --  That's why it cannot be applied to Test_Primarity in pacakge
   --  Common or Prime. Moreover, when we apply pragma Asynchronous
   --  to Pool_Access, all subprograms dispatching on Pool_Type'Class
   --  with only 'in' parameters become asynchronous RPCs.

   function Register
     (Pool : Pool_Access;
      PID  : Partition_ID)
     return Boolean;

   function  Next
     (PID : Partition_ID)
      return Pool_Access;

end Controller;
