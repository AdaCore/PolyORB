
with Ada.Streams;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with PolyORB.Buffers;
with PolyORB.Components;

with PolyORB.Types; use PolyORB.Types;



package PolyORB.Filters.HTTP is

   pragma Elaborate_Body;

   type HTTP_Filter_Factory is new Factory with private;

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access);

   Unexpected_Data : exception;
   --  Raised when unexpected data is received by this filter.

private

   Char_Length : constant Ada.Streams.Stream_Element_Offset := 1;

   Str_CRLF : constant Types.String := To_PolyORB_String (CR & LF & CR & LF);

   type HTTP_Filter_Factory is new Factory with null record;

   type HTTP_Filter is new Filter with record
      In_Buf        : Buffers.Buffer_Access;
      Data_Expected : Ada.Streams.Stream_Element_Count;
      Buffer_Length : Ada.Streams.Stream_Element_Count;
      Expected_Size_Fixed  : Boolean := False;
   end record;

   function Handle_Message
     (F : access HTTP_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

end PolyORB.Filters.HTTP;
