with CORBA; use CORBA;


package Helper is

   --  Example  --

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


   function To_Any (From : in Example) return Any;
   function From_Any (From : Any) return Example;


   -- Simple_Struct
   type a_Array is array (0 .. 9) of CORBA.Long;
   type a_Array_Ptr is access a_Array;
   type simple_struct is record
      A : a_Array;
      B : CORBA.Long;
   end record;
   
   function To_Any (From : in Simple_Struct) return Any;
   function From_Any (From : in Any) return Simple_Struct;
      
   --  Color  --
   type Color is
      (Red, Green, Blue);
   
   function To_Any (From : in Color) return Any;
   function From_Any (From : in Any) return Color;

   --  Line  --
   type Line is array (0 .. 2) of Example;

   function To_Any (From : in Line) return Any;
   function From_Any (From : in Any) return Line;
   
   --  Square  --
   type Square is array (0 .. 1, 0 .. 1) of simple_struct;

   function To_Any (From : in Square) return Any;
   function From_Any (From : in Any) return Square;

   --  Cube  --
   type Cube is array (0 .. 1, 0 .. 1, 0 .. 1) of CORBA.String;
   
   function To_Any (From : in Cube) return Any;
   function From_Any (From : in Any) return Cube;

end Helper;

