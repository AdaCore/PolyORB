--  A dummy data representation method, just for show.

--  $Id$

with PolyORB.Buffers; use PolyORB.Buffers;

package PolyORB.Representations.Test is

   pragma Elaborate_Body;

   type Rep_Test is new Representation with private;
   type Rep_Test_Access is access all Rep_Test;

   --  A real representation function should implement the
   --  following two subprograms.

   procedure Marshall_From_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any);

   procedure Unmarshall_To_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any);

   --  The following methods are specific to Rep_Test and are
   --  here only to facilitate testing of other parts of the ORB.

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character);
   --  Marshall one character.

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character;
   --  Unmarshall one character.

   procedure Marshall_String
     (R : access Rep_Test;
      B : access Buffer_Type;
      S : String);
   --  Marshall a string.

   function Unmarshall_String
     (R : access Rep_Test;
      B : access Buffer_Type)
     return String;
   --  Unmarshall a string terminated by a CR/LF sequence.

private

   type Rep_Test is new Representation with null record;

end PolyORB.Representations.Test;
