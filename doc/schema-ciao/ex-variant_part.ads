type Colour is
   (Red, Green, Blue, Yellow);

type My_Record (D : Colour) is
   record
      Foo : Integer;
      case D is
         when Red =>
            Bar : String;
            Baz : Integer;
         when Green
           .. Blue =>
            Fuz : Integer;
         when others =>
            null;
      end case;
   end record;
