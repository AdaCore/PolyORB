with CORBA; use CORBA;


--  The functions in this package are responsible for generation of constant
--  typecodes, form and to any conversions, for the complex user defined types.
--
--  It probably looks very like what the adabroker IDL to Ada compiler should
--  be able to produce, as a 'helper file' for the dynamic client.
--
--  The use of an IR would probably also allow the genewration of suc TypeCode
--  constants.
--
--  You may want to look at the implementation of the TC_XXX functions to
--  convince yourself of how useful it would be ...


package Helper is

   function TC_String return TypeCode.Object;
   --  returns a type code for an unbounded string

   ---------------
   --  Example  --
   ---------------

   type Example(Switch : CORBA.Long := CORBA.Long'First) is
      record
         case Switch is
            when 1 =>
               Counter : CORBA.Long;
            when 2 =>
               Flags : CORBA.Boolean;
            when others =>
               Unknown : CORBA.Long;
         end case;
      end record;

   function TC_Example return TypeCode.Object;
   function To_Any (From : in Example) return Any;
   function From_Any (From : Any) return Example;


   -------------------
   --  simple array --
   -------------------

   type Simple_Array is  array (0 .. 9) of CORBA.Long;
   type Simple_Array_Ptr is access Simple_Array;

   function TC_Simple_Array return TypeCode.Object;
   function To_Any (From : in Simple_Array) return Any;
   function From_Any (From : in Any) return Simple_Array;


   -------------------
   -- Simple_Struct --
   -------------------

   type a_Array is array (0 .. 9) of CORBA.Long;
   type a_Array_Ptr is access a_Array;
   type simple_struct is record
      A : a_Array;
      B : CORBA.Long;
   end record;

   function TC_A_Array return TypeCode.Object;
   function To_Any (From : in A_Array) return Any;
   function From_Any (From : in Any) return A_Array;

   function TC_Simple_Struct return TypeCode.Object;
   function To_Any (From : in Simple_Struct) return Any;
   function From_Any (From : in Any) return Simple_Struct;


   -------------
   --  Color  --
   -------------

   type Color is
      (Red, Green, Blue);

   function TC_Color return TypeCode.Object;
   function To_Any (From : in Color) return Any;
   function From_Any (From : in Any) return Color;


   ------------
   --  Line  --
   ------------

   type Line is array (0 .. 2) of Example;

   function TC_Line return TypeCode.Object;
   function To_Any (From : in Line) return Any;
   function From_Any (From : in Any) return Line;

   --------------
   --  Square  --
   --------------

   type Square is array (0 .. 1, 0 .. 1) of simple_struct;

   function TC_Square return TypeCode.Object;
   function To_Any (From : in Square) return Any;
   function From_Any (From : in Any) return Square;

   ------------
   --  Cube  --
   ------------

   type Cube is array (0 .. 1, 0 .. 1, 0 .. 1) of CORBA.String;

   function TC_Cube return TypeCode.Object;
   function To_Any (From : in Cube) return Any;
   function From_Any (From : in Any) return Cube;

end Helper;

