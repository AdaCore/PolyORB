
     --==================================================================--

 
package body ImpDef is
 
   -- NOTE: These are example bodies.  It is expected that implementors
   --       will write their own versions of these routines.
 
--=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====--
 
   -- The time required to execute this procedure must be greater than the
   -- time slice unit on implementations which use time slicing.  For
   -- implementations which do not use time slicing the body can be null.

   Procedure Exceed_Time_Slice is
      T : Integer := 0;
      Loop_Max : constant Integer := 4_000;
   begin
      for I in 1..Loop_Max loop
         T := Report.Ident_Int (1) * Report.Ident_Int (2);
      end loop;
   end Exceed_Time_Slice;
 
--=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====--

end ImpDef;
