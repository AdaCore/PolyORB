with Types;    use Types;

package body Ptypes is

   protected S is
      entry Write (Node : in  Node_Access);
      entry Read  (Node : out Node_Access);
   private
      X : Node_Access := null;
   end S;

   protected body S is
      entry Write (Node : in  Node_Access) when X = null is
      begin
	 X := Node;
      end Write;
      entry Read  (Node : out Node_Access) when X /= null is
      begin
	 Node := X;
      end Read;
   end S;

   procedure Register (A : Node_Access) is
   begin
     S.Write (A);
   end Register;
   
   function Get_Node return Node_Access is
      A : Node_Access;
   begin
      S.Read (A);
      return A;
   end Get_Node;

end Ptypes;
