with Types;   use Types;
with Ptypes;  use Ptypes;

package Etypes is

   type New_Node_Type;

   type Tmp_Node_Type (Self : access New_Node_Type) is limited null record;

   type New_Node_Type is new Node_Type with
      record
         My   : Tmp_Node_Type (New_Node_Type'Access);
         Data : Integer;
      end record;

   procedure Print (N : access New_Node_Type);

end Etypes;
