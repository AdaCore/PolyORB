generic
   type Element is private;
   Null_Element : Element;
   type Index_Type is range <>;
   Initial_Size : Index_Type := 10;
   First : Index_Type := Index_Type'First;
package Broca.Vararray is
   type Var_Array_Type is private;
   Null_Var_Array : constant Var_Array_Type;

   type Iterator_Type is access procedure (El : Element; Arg : Boolean);

   procedure Insert (Varray : in out Var_Array_Type; El : Element);
   procedure Remove (Varray : in out Var_Array_Type; El : Element);
   procedure Iterate (Varray : in out Var_Array_Type;
                      Iterator : Iterator_Type;
                      Arg : Boolean);
   function Get_Element (Varray : Var_Array_Type; Index : Index_Type)
     return Element;
   function Bad_Index (Varray : Var_Array_Type; Index : Index_Type)
     return Boolean;
private
   type Element_Array is array (Index_Type range <>) of Element;
   type Var_Array_Type is access Element_Array;
   Null_Var_Array : constant Var_Array_Type := null;
end Broca.Vararray;
