with Ada.Streams;
with System.Garlic.Streams;

package System.Garlic.Filters.Reversing is

private

   type New_Filter_Type is new Filter_Type with null record;

   type New_Filter_Params_Type is new Filter_Params_Type with null record;

   function Filter_Outgoing
     (Filter : in     New_Filter_Type;
      Params : in     Filter_Params_Access;
      Stream : access System.Garlic.Streams.Params_Stream_Type)
      return Streams.Stream_Element_Access;

   function Filter_Incoming
     (Filter : in New_Filter_Type;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Streams.Stream_Element_Access;

   procedure Generate_Params
     (Filter          : in  New_Filter_Type;
      Public_Params   : out Filter_Params_Access;
      Private_Params  : out Filter_Params_Access;
      Exchange_Params : out Boolean);

   function Filter_Params_Read
     (Filter : New_Filter_Type;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access;

   function Filter_Params_Write
     (Filter : New_Filter_Type;
      Params : Filter_Params_Access)
      return Streams.Stream_Element_Access;

end System.Garlic.Filters.Reversing;
