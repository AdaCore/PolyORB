with Ada.Streams; use Ada.Streams;

with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);

with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Filters.Doubling is

   New_Filter : aliased New_Filter_Type;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     New_Filter_Type;
      Params : in     Filter_Params_Access;
      Stream : access System.Garlic.Streams.Params_Stream_Type)
      return Stream_Element_Access is
      R : Stream_Element_Access  := To_Stream_Element_Access (Stream);
      D : Stream_Element_Access  := new Stream_Element_Array'(R.all & R.all);
   begin
      for I in R'Range loop
         D (D'First + (I - D'First) * 2)     := Stream_Element'First;
         D (D'First + (I - D'First) * 2 + 1) := R (I);
      end loop;
      Free (R);
      return D;
   end Filter_Outgoing;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
     (Filter : in New_Filter_Type;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is
      F : Stream_Element_Count := Stream'First;
      L : Stream_Element_Count := (Stream'First + Stream'Last) / 2;
      R : Stream_Element_Access := new Stream_Element_Array (F .. L);
   begin
      for I in Stream'Range loop
         R ((I - F) / 2 + F) := Stream (I);
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
   Register_Filter (New_Filter'Access, "doubling");
end System.Garlic.Filters.Doubling;
