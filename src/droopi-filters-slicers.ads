--  A filter that slices a stream into a set of known-length
--  messages.

--  $Id$

with Ada.Streams;

with Droopi.Buffers;
with Droopi.Components;

package Droopi.Filters.Slicers is

   pragma Elaborate_Body;

   type Slicer_Factory is new Factory with private;

   procedure Create
     (Fact   : access Slicer_Factory;
      Slicer : out Filter_Access);

   Unexpected_Data : exception;
   --  Raised when unexpected data is received by this filter.

private

   type Slicer_Factory is new Factory with null record;

   type Slicer_Filter is new Filter with record
      In_Buf        : Buffers.Buffer_Access;
      Data_Expected : Ada.Streams.Stream_Element_Count;
      Buffer_Length : Ada.Streams.Stream_Element_Count;
   end record;

   function Handle_Message
     (F : access Slicer_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

end Droopi.Filters.Slicers;
