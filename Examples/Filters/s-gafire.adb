with Ada.Streams; use Ada.Streams;

with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);

with System.Garlic.Utils;   use System.Garlic.Utils;
with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Filters.Reversing is

   Dummy : aliased No_Filter;
   --  Dummy instance of No_Filter, only used for dispatching

   Dummy_Params : aliased No_Filter_Params;
   --  Dummy filter parameters, allowing the use of this filter even as
   --  the default filter.

   Dummy_Name : constant String := "reversing";

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     No_Filter;
      Params : in     Filter_Params_Access;
      Stream : access System.RPC.Params_Stream_Type)
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
     (Filter : in No_Filter;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is
      Result  : Stream_Element_Access := new Stream_Element_Array'(Stream);
   begin
      for I in Result'Range loop
         Result (I) := Stream_Element'Last - Result (I);
      end loop;
      return Result;
   end Filter_Incoming;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (Filter : in No_Filter;
      Params : out Filter_Params_Access;
      Private_Params : out Filter_Params_Access;
      Needs_Key_Exchange : out Boolean) is
   begin
      Params := Dummy_Params'Access;
      Private_Params := Dummy_Params'Access;
      Needs_Key_Exchange := false;
   end Generate_Params;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
     (Filter : No_Filter;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is
      S : aliased System.RPC.Params_Stream_Type (Stream'Length);
      P : No_Filter_Params;
   begin
      To_Params_Stream_Type (Stream, S'Access);
      No_Filter_Params'Read (S'Access, P);
      return new No_Filter_Params'(P);
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
     (Filter : No_Filter;
      P : Filter_Params_Access) return
      Streams.Stream_Element_Access is
      S : aliased System.RPC.Params_Stream_Type (32);
   begin
      No_Filter_Params'Write (S'Access, No_Filter_Params (P.all));
      return To_Stream_Element_Access (S'Access);
   end Filter_Params_Write;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Filter : No_Filter)
      return String is
   begin
      return Dummy_Name;
   end Get_Name;

   ------------------
   -- Print_Params --
   ------------------

   procedure Print_Params (Params : No_Filter_Params) is
   begin
      null;
   end Print_Params;


begin
   Register_Filter (Dummy'Access);
end System.Garlic.Filters.Reversing;
