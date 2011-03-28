with Ada.Streams; use Ada.Streams;
with Ada.Numerics.Discrete_Random;

with System.Garlic.Debug;   use System.Garlic.Debug;
with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);
with System.Garlic.Streams; use System.Garlic.Streams;
with System.Garlic.Types; use System.Garlic.Types;

package body System.Garlic.Filters.Shift is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("SHIFT", "(s-gafish): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package Shift_Filter_Params is
     new Ada.Numerics.Discrete_Random (Stream_Element);

   Generator : Shift_Filter_Params.Generator;

   Shift_Filter : aliased Shift_Filter_Type;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
     (Filter : in Shift_Filter_Type;
      Params : in Filter_Params_Access;
      Stream : in Stream_Element_Access;
      Offset : in Stream_Element_Offset)
      return Stream_Element_Access
   is
      pragma Unreferenced (Filter);
      Shift_Params  : Shift_Filter_Params_Type;
      Target_Buffer : Stream_Element_Access;

      F : constant Stream_Element_Offset := Stream'First + Offset;
      L : constant Stream_Element_Offset := Stream'Last;

   begin
      D ("Entering shift incoming filter");

      Shift_Params  := Shift_Filter_Params_Type (Params.all);
      D ("Use shift params" & Shift_Params.Times'Img);

      Target_Buffer := new Stream_Element_Array'(Stream (F .. L));
      Dump (Target_Buffer, Private_Debug_Key);

      for I in Target_Buffer'Range loop
         Target_Buffer (I) := Target_Buffer (I) xor Shift_Params.Times;
      end loop;

      D ("Leaving shift incoming filter");
      Dump (Target_Buffer, Private_Debug_Key);

      return Target_Buffer;
   end Filter_Incoming;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     Shift_Filter_Type;
      Params : in     Filter_Params_Access;
      Stream : access System.Garlic.Streams.Params_Stream_Type)
     return Stream_Element_Access
   is
      pragma Unreferenced (Filter);
      Shift_Params  : Shift_Filter_Params_Type;
      Target_Buffer : Stream_Element_Access;

   begin
      D ("Entering shift outgoing filter");

      Shift_Params  := Shift_Filter_Params_Type (Params.all);
      D ("Use shift params" & Shift_Params.Times'Img);

      Target_Buffer := To_Stream_Element_Access (Stream);
      Dump (Target_Buffer, Private_Debug_Key);

      for I in Target_Buffer'Range loop
         Target_Buffer (I) := Target_Buffer (I) xor Shift_Params.Times;
      end loop;

      D ("Leaving shift outgoing filter");
      Dump (Target_Buffer, Private_Debug_Key);

      return Target_Buffer;
   end Filter_Outgoing;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
     (Filter : Shift_Filter_Type;
      Stream : Ada.Streams.Stream_Element_Array)
     return Filter_Params_Access
   is
      pragma Unreferenced (Filter);
   begin
      D ("Read filter params" & Stream (Stream'First)'Img);
      return new Shift_Filter_Params_Type'(Times => Stream (Stream'First));
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
     (Filter : Shift_Filter_Type;
      Params : Filter_Params_Access) return
     Streams.Stream_Element_Access
   is
      pragma Unreferenced (Filter);
      Shift_Filter_Params : Shift_Filter_Params_Type;

   begin
      Shift_Filter_Params := Shift_Filter_Params_Type (Params.all);
      D ("Write filter params" & Shift_Filter_Params.Times'Img);
      return new Stream_Element_Array'(1 .. 1 => Shift_Filter_Params.Times);
   end Filter_Params_Write;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (Filter          : in Shift_Filter_Type;
      Public_Params   : out Filter_Params_Access;
      Private_Params  : out Filter_Params_Access;
      Exchange_Params : out Boolean)
   is
      pragma Unreferenced (Filter);
      Element : Stream_Element;

   begin
      for I in 1 .. Self_PID loop
         Element := Shift_Filter_Params.Random (Generator);
      end loop;
      D ("Generate shift params" & Element'Img);
      Public_Params   := new Shift_Filter_Params_Type'(Times => Element);
      Private_Params  := new Shift_Filter_Params_Type'(Times => Element);
      Exchange_Params := True;
   end Generate_Params;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Filter (Shift_Filter'Access, "shift");
   end Initialize;

end System.Garlic.Filters.Shift;
