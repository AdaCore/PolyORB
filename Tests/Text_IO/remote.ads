--
--  remote.ads,v 1.2 1996/04/10 15:42:59 tardieu Exp
--

package Remote is

   pragma Remote_Call_Interface;

   function Double (X : Natural) return Natural;
   --  This procedure will do a call to Text_IO routines to make sure that
   --  the partition doesn't crash when such a call is made. It then returns
   --  twice the number given as argument.

end Remote;
