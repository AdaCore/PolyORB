with Common; use Common;
package Controller is

   pragma Remote_Call_Interface;
   type Prime_Pool_Access is access all Prime_Pool_Type'Class;

   procedure Register
     (Pool  : in  Prime_Pool_Access;
      Index : out Natural);
   function  Next
     (Index : Natural)
      return Prime_Pool_Access;
   function First
      return Prime_Pool_Access;

end Controller;
