--  A filter that slices a stream into a set of known-length
--  messages.

--  $Id$

with Ada.Streams;

with Droopi.Buffers;

package Droopi.Filters.Slicers is

   pragma Elaborate_Body;

   type Slicer_Factory is new Factory with private;

   procedure Create
     (Fact   : access Slicer_Factory;
      Slicer : out Filter_Access);

   Unexpected_Data : exception;
   --  Raised when unexpected data is received by this filter.

private

   type Slicer_Filter is new Filter with record
      In_Buf : Buffer_Access;
      Data_Expected : Stream_Element_Count;
   end record;

   procedure Handle_Message
     (F : access Slicer_Filter;
      S : Data_Unit);

end Droopi.Filters.Slicers;
