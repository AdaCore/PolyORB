with Ada.Streams;
with System.Garlic.Streams;

package System.Garlic.Filters.Shuffling is

private

   type No_Filter is new Filter_Type with null record;

   type No_Filter_Params is new Filter_Params with null record;

   function Filter_Outgoing
     (Filter : in     No_Filter;
      Params : in     Filter_Params_Access;
      Stream : access System.RPC.Params_Stream_Type)
      return Streams.Stream_Element_Access;

   function Filter_Incoming
     (Filter : in No_Filter;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Streams.Stream_Element_Access;

   procedure Generate_Params
     (Filter : in No_Filter;
      Params : out Filter_Params_Access;
      Private_Params : out Filter_Params_Access;
      Needs_Key_Exchange : out Boolean);

   function Filter_Params_Read
     (Filter : No_Filter;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access;

   function Filter_Params_Write
     (Filter : No_Filter;
      P : Filter_Params_Access)
      return Streams.Stream_Element_Access;

   function Get_Name
     (Filter : No_Filter)
     return String;

   procedure Print_Params (Params : No_Filter_Params);

end System.Garlic.Filters.Shuffling;
