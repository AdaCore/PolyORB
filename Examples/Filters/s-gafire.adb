with Ada.Streams; use Ada.Streams;

with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);

with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Filters.Reversing is

   New_Filter : aliased New_Filter_Type;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     New_Filter_Type;
      Params : in     Filter_Params_Access;
      Stream : access System.Garlic.Streams.Params_Stream_Type)
      return Stream_Element_Access is
      Result  : Stream_Element_Access  := To_Stream_Element_Access (Stream);
   begin
      for I in Result'Range loop
         Result (I) := Stream_Element'Last - Result (I);
      end loop;
      return Result;
   end Filter_Outgoing;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
     (Filter : in New_Filter_Type;
      Params : in Filter_Params_Access;
      Stream : in Stream_Element_Access;
      Offset : in Stream_Element_Offset)
      return Stream_Element_Access
   is
      F : constant Stream_Element_Offset := Stream'First + Offset;
      L : constant Stream_Element_Offset := Stream'Last;
      R : Stream_Element_Access := new Stream_Element_Array'(Stream (F .. L));
   begin
      for I in R'Range loop
         R (I) := Stream_Element'Last - R (I);
      end loop;
      return R;
   end Filter_Incoming;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (Filter          : in  New_Filter_Type;
      Public_Params   : out Filter_Params_Access;
      Private_Params  : out Filter_Params_Access;
      Exchange_Params : out Boolean) is
   begin
      Public_Params   := null;
      Private_Params  := null;
      Exchange_Params := False;
   end Generate_Params;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
     (Filter : New_Filter_Type;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is
   begin
      return null;
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
     (Filter : New_Filter_Type;
      Params : Filter_Params_Access) return
      Streams.Stream_Element_Access is
   begin
      return null;
   end Filter_Params_Write;

begin
   Register_Filter (New_Filter'Access, "reversing");
end System.Garlic.Filters.Reversing;
