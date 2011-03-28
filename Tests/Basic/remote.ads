--
--  remote.ads,v 1.2 1996/04/10 15:42:48 tardieu Exp
--

package Remote is

   pragma Remote_Call_Interface (Remote);

   function Revert (S : String) return String;
   --  Revert the string given as parameter

end Remote;
