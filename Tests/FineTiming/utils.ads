with Ada.Streams;
with Interfaces.C;

package Utils is

   procedure Send
     (Sock : in Interfaces.C.int;
      Data : access Ada.Streams.Stream_Element_Array);

   procedure Receive
     (Sock : in Interfaces.C.int;
      Data : access Ada.Streams.Stream_Element_Array);

end Utils;
