with Types; use Types;

package Backend.BE_IDL is

   procedure Generate (E : Node_Id);
   procedure Configure;
   procedure Usage (Indent : Natural);

   Print_IDL_Tree       : Boolean := False;

end Backend.BE_IDL;
