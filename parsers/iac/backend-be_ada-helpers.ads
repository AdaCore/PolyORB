with Types; use Types;

package Backend.BE_Ada.Helpers is

   package Package_Spec is

      procedure Any_Conversion_Spec (T : Node_Id);
      --  Insert an any conversions functions for a given type
      --  (T) node in the helper package.
      procedure Narrowing_Ref_Spec;
      --  Insert windening object reference helper.
      procedure TypeCode_Spec (T : Node_Id);
      --  Insert a TypeCode constant for a given type (T) node in the Helper
      --  package.
      procedure Widening_Ref_Spec;
      --  Insert widening object reference helper.

      procedure Visit (E : Node_Id);

   end Package_Spec;

   package Package_Body is
      procedure Any_Conversion_Body (T : Node_Id);
      --  Insert an any conversions functions for a given type
      --  (T) node in the helper package.
      procedure Narrowing_Ref_Body;
      --  Insert windening object reference helper.
      procedure TypeCode_Body (T : Node_Id);
      --  Insert a TypeCode constant for a given type (T) node in the Helper
      --  package.
      procedure Widening_Ref_Body;
      --  Insert widening object reference helper.

      procedure Visit (E : Node_Id);
   end Package_Body;

end Backend.BE_Ada.Helpers;
