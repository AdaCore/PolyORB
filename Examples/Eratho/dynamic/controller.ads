with Common; use Common;

package Controller is

   pragma Remote_Call_Interface;
   type Pool_Access is access all Pool_Type'Class;

   function Register
     (Pool : Pool_Access;
      PID  : Partition_ID)
     return Boolean;

   function  Next
     (PID : Partition_ID)
      return Pool_Access;

end Controller;
