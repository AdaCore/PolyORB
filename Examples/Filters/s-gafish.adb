with Ada.Streams; use Ada.Streams;
with Ada.Numerics.Discrete_Random;

with System.Garlic.Debug;   use System.Garlic.Debug;
with System.Garlic.Heart;   use System.Garlic.Heart;
with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);
with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Filters.Shuffling is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("SHUFFLING", "(s-gafish): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package Shuffling_Filter_Params is
     new Ada.Numerics.Discrete_Random (Stream_Element);

   Generator : Shuffling_Filter_Params.Generator;

   Shuffling_Filter : aliased Shuffling_Filter_Type;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
     (Filter : in Shuffling_Filter_Type;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is
      Shuffling_Params  : Shuffling_Filter_Params_Type;
      Target_Buffer : Stream_Element_Access;

   begin
      D (D_Debug, "Entering shuffling incoming filter");

      Shuffling_Params  := Shuffling_Filter_Params_Type (Params.all);
      D (D_Debug, "Use shuffling params" & Shuffling_Params.Times'Img);

      Target_Buffer := new Stream_Element_Array'(Stream);
      Dump (D_Debug, Target_Buffer, Private_Debug_Key);

      for I in Target_Buffer'Range loop
         Target_Buffer (I) := Target_Buffer (I) xor Shuffling_Params.Times;
      end loop;

      D (D_Debug, "Leaving shuffling incoming filter");
      Dump (D_Debug, Target_Buffer, Private_Debug_Key);

      return Target_Buffer;
   end Filter_Incoming;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     Shuffling_Filter_Type;
      Params : in     Filter_Params_Access;
      Stream : access System.Garlic.Streams.Params_Stream_Type)
      return Stream_Element_Access is
      Shuffling_Params  : Shuffling_Filter_Params_Type;
      Target_Buffer : Stream_Element_Access;

   begin
      D (D_Debug, "Entering shuffling outgoing filter");

      Shuffling_Params  := Shuffling_Filter_Params_Type (Params.all);
      D (D_Debug, "Use shuffling params" & Shuffling_Params.Times'Img);

      Target_Buffer := To_Stream_Element_Access (Stream);
      Dump (D_Debug, Target_Buffer, Private_Debug_Key);

      for I in Target_Buffer'Range loop
         Target_Buffer (I) := Target_Buffer (I) xor Shuffling_Params.Times;
      end loop;

      D (D_Debug, "Leaving shuffling outgoing filter");
      Dump (D_Debug, Target_Buffer, Private_Debug_Key);

      return Target_Buffer;
   end Filter_Outgoing;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
     (Filter : Shuffling_Filter_Type;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is
   begin
      D (D_Debug, "Read filter params" & Stream (Stream'First)'Img);
      return new Shuffling_Filter_Params_Type'(Times => Stream (Stream'First));
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
     (Filter : Shuffling_Filter_Type;
      Params : Filter_Params_Access) return
      Streams.Stream_Element_Access is
      Shuffling_Filter_Params : Shuffling_Filter_Params_Type;

   begin
      Shuffling_Filter_Params := Shuffling_Filter_Params_Type (Params.all);
      D (D_Debug, "Write filter params" & Shuffling_Filter_Params.Times'Img);
      return
         new Stream_Element_Array'(1 .. 1 => Shuffling_Filter_Params.Times);
   end Filter_Params_Write;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (Filter          : in Shuffling_Filter_Type;
      Public_Params   : out Filter_Params_Access;
      Private_Params  : out Filter_Params_Access;
      Exchange_Params : out Boolean) is
      Element : Stream_Element;

   begin
      for I in 1 .. Get_My_Partition_ID loop
         Element := Shuffling_Filter_Params.Random (Generator);
      end loop;
      D (D_Debug, "Generate shuffling params" & Element'Img);
      Public_Params   := new Shuffling_Filter_Params_Type'(Times => Element);
      Private_Params  := new Shuffling_Filter_Params_Type'(Times => Element);
      Exchange_Params := True;
   end Generate_Params;

begin
   Register_Filter (Shuffling_Filter'Access, "shuffling");
end System.Garlic.Filters.Shuffling;
