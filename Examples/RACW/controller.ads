with Common; use Common;
package Controller is
   pragma Remote_Call_Interface;
   type Worker_Access is access all Worker'Class;
   procedure Register (W : Worker_Access);
   function  Get_Worker return Worker_Access;
   procedure Get_Integer (Message : String; Value : out Integer);
   procedure Done (Message : String);
end Controller;
