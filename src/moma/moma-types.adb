------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           M O M A . T Y P E S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body MOMA.Types is

   use PolyORB.Any;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("moma.types");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Standard.Boolean
     renames L.Enabled;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : MOMA.Types.Any) return Map_Element
   is
      Index  : PolyORB.Any.Any;
      Result : Map_Element;

   begin
      pragma Debug (C, O ("From_Any : (Map_Element)"));
      Index := Get_Aggregate_Element (Item,
                                      TypeCode.TC_String,
                                      PolyORB.Types.Unsigned_Long (0));
      Result.Name := MOMA.Types.String
        (PolyORB.Types.String'(PolyORB.Any.From_Any (Index)));

      Result.Value := From_Any (Get_Aggregate_Element
                                (Item,
                                 TypeCode.TC_Any,
                                 PolyORB.Types.Unsigned_Long (1)));

      return Result;
   end From_Any;

   function From_Any
     (Item : MOMA.Types.Any)
     return IDL_SEQUENCE_Map_Element.Sequence
   is
      use IDL_SEQUENCE_Map_Element;
      Nb_Any : constant Any :=
                 Get_Aggregate_Element (Item,
                   TC_Unsigned_Long, PolyORB.Types.Unsigned_Long (0));

      Nb_Long : constant Unsigned_Long := From_Any (Nb_Any);
      Nb      : constant Integer := Integer (Nb_Long);
      Index   : Any;
      Result  : Element_Array (1 .. Nb);

   begin
      pragma Debug (C, O ("From_Any : (IDL_Sequence_Map_Element)"));

      for J in 1 .. Nb loop
         Index := Get_Aggregate_Element (Item,
                                         TC_Map_Element,
                                         PolyORB.Types.Unsigned_Long (J));
         Result (J) := From_Any (Index);
      end loop;

      return To_Sequence (Result);
   end From_Any;

   function From_Any (Item : MOMA.Types.Any) return Map is
      Result : constant IDL_SEQUENCE_Map_Element.Sequence := From_Any (Item);
   begin
      pragma Debug (C, O ("From_Any : (Map)"));
      return Map (Result);
   end From_Any;

   function From_Any (Item : MOMA.Types.Any) return Destination_Type
   is
      Index : constant Any :=
                 Get_Aggregate_Element (Item,
                   TC_Unsigned_Long, PolyORB.Types.Unsigned_Long (0));
      Position : constant Unsigned_Long := From_Any (Index);
   begin
      return Destination_Type'Val (Position);
   end From_Any;

   function From_Any (Item : Any) return Short is
   begin
      return Short
        (PolyORB.Types.Short'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Long is
   begin
      return Long
        (PolyORB.Types.Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Unsigned_Short is
   begin
      return Unsigned_Short
        (PolyORB.Types.Unsigned_Short'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Unsigned_Long is
   begin
      return Unsigned_Long
        (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return MOMA.Types.Float is
   begin
      return MOMA.Types.Float
        (PolyORB.Types.Float'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Double is
   begin
      return Double
        (PolyORB.Types.Double'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Boolean is
   begin
      return Boolean
        (PolyORB.Types.Boolean'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Char is
   begin
      return Char
        (PolyORB.Types.Char'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Byte is
   begin
      return Byte
        (PolyORB.Types.Octet'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Any
     renames PolyORB.Any.From_Any;

   function From_Any (Item : Any) return MOMA.Types.String is
   begin
      return MOMA.Types.String
        (PolyORB.Types.String'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Map_Element) return MOMA.Types.Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TC_Map_Element);

   begin
      pragma Debug (C, O ("To_Any : (Map_Element)"));
      Add_Aggregate_Element (Result, To_Any (Item.Name));
      Add_Aggregate_Element (Result, To_Any (Item.Value));

      return Result;
   end To_Any;

   function To_Any
     (Item : IDL_SEQUENCE_Map_Element.Sequence)
     return MOMA.Types.Any
   is
      use IDL_SEQUENCE_Map_Element;
      Array_Item : constant Element_Array := To_Element_Array (Item);
      Result : Any := Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_Map_Element);

   begin
      pragma Debug (C, O ("To_Any : (IDL_SEQUENCE_Map_Element)"));
      Add_Aggregate_Element
        (Result, To_Any (Unsigned_Long (Length (Item))));

      for J in Array_Item'Range loop
         Add_Aggregate_Element (Result,
                                To_Any (Array_Item (J)));
      end loop;

      return Result;
   end To_Any;

   function To_Any (Item : Map) return MOMA.Types.Any
   is
      Result : Any := To_Any (IDL_SEQUENCE_Map_Element.Sequence (Item));
   begin
      pragma Debug (C, O ("To_Any : (Map)"));
      Set_Type (Result, TC_Map);
      return Result;
   end To_Any;

   function To_Any (Item : Destination_Type) return MOMA.Types.Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TC_Destination_Type);
   begin
      Add_Aggregate_Element
        (Result,
         To_Any (Unsigned_Long (Destination_Type'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any (Item : Short) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Short (Item));
   end To_Any;

   function To_Any (Item : Long) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Long (Item));
   end To_Any;

   function To_Any (Item : Unsigned_Short) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (Item));
   end To_Any;

   function To_Any (Item : Unsigned_Long) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Item));
   end To_Any;

   function To_Any (Item : MOMA.Types.Float) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Float (Item));
   end To_Any;

   function To_Any (Item : Double) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Double (Item));
   end To_Any;

   function To_Any (Item : Boolean) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Boolean (Item));
   end To_Any;

   function To_Any (Item : Char) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Char (Item));
   end To_Any;

   function To_Any (Item : Byte) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Octet (Item));
   end To_Any;

   function To_Any (Item : Any) return Any
     renames PolyORB.Any.To_Any;

   function To_Any (Item : MOMA.Types.String) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.String (Item));
   end To_Any;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Self : Map_Element)
     return MOMA.Types.Boolean is
   begin
      return From_Any (Self.Value);
   end Get_Boolean;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Boolean) is
   begin
      Self.Value := To_Any (PolyORB.Types.Boolean (Value));
   end Set_Boolean;

   --------------
   -- Get_Byte --
   --------------

   function Get_Byte
     (Self : Map_Element)
     return MOMA.Types.Byte is
   begin
      return From_Any (Self.Value);
   end Get_Byte;

   --------------
   -- Set_Byte --
   --------------

   procedure Set_Byte
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Byte) is
   begin
      Self.Value := To_Any (PolyORB.Types.Octet (Value));
   end Set_Byte;

   --------------
   -- Get_Char --
   --------------

   function Get_Char
     (Self : Map_Element)
     return MOMA.Types.Char is
   begin
      return From_Any (Self.Value);
   end Get_Char;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Char) is
   begin
      Self.Value := To_Any (PolyORB.Types.Char (Value));
   end Set_Char;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double
     (Self : Map_Element)
     return MOMA.Types.Double is
   begin
      return From_Any (Self.Value);
   end Get_Double;

   ----------------
   -- Set_Double --
   ----------------

   procedure Set_Double
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Double) is
   begin
      Self.Value := To_Any (PolyORB.Types.Double (Value));
   end Set_Double;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float
     (Self : Map_Element)
     return MOMA.Types.Float is
   begin
      return From_Any (Self.Value);
   end Get_Float;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Float) is
   begin
      Self.Value := To_Any (PolyORB.Types.Float (Value));
   end Set_Float;

   --------------
   -- Get_Long --
   --------------

   function Get_Long
     (Self : Map_Element)
     return MOMA.Types.Long is
   begin
      return From_Any (Self.Value);
   end Get_Long;

   --------------
   -- Set_Long --
   --------------

   procedure Set_Long
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Long) is
   begin
      Self.Value := To_Any (PolyORB.Types.Long (Value));
   end Set_Long;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : Map_Element)
     return MOMA.Types.String is
   begin
      return Self.Name;
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Self  : in out Map_Element;
      Value :        MOMA.Types.String) is
   begin
      Self.Name := Value;
   end Set_Name;

   ---------------
   -- Get_Short --
   ---------------

   function Get_Short
     (Self : Map_Element)
     return MOMA.Types.Short is
   begin
      return From_Any (Self.Value);
   end Get_Short;

   ---------------
   -- Set_Short --
   ---------------

   procedure Set_Short
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Short) is
   begin
      Self.Value := To_Any (PolyORB.Types.Short (Value));
   end Set_Short;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Self : Map_Element)
     return MOMA.Types.String is
   begin
      return From_Any (Self.Value);
   end Get_String;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (Self  : in out Map_Element;
      Value :        MOMA.Types.String) is
   begin
      Self.Value := To_Any (PolyORB.Types.String (Value));
   end Set_String;

   -----------------------
   -- Get_Unsigned_Long --
   -----------------------

   function Get_Unsigned_Long
     (Self : Map_Element)
     return MOMA.Types.Unsigned_Long is
   begin
      return From_Any (Self.Value);
   end Get_Unsigned_Long;

   -----------------------
   -- Set_Unsigned_Long --
   -----------------------

   procedure Set_Unsigned_Long
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Unsigned_Long) is
   begin
      Self.Value := To_Any (PolyORB.Types.Unsigned_Long (Value));
   end Set_Unsigned_Long;

   ------------------------
   -- Get_Unsigned_Short --
   ------------------------

   function Get_Unsigned_Short
     (Self : Map_Element)
     return MOMA.Types.Unsigned_Short is
   begin
      return From_Any (Self.Value);
   end Get_Unsigned_Short;

   ------------------------
   -- Set_Unsigned_Short --
   ------------------------

   procedure Set_Unsigned_Short
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Unsigned_Short) is
   begin
      Self.Value := To_Any (PolyORB.Types.Unsigned_Short (Value));
   end Set_Unsigned_Short;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Pool : MOMA.Types.Message_Pool)
     return MOMA.Types.String is
   begin
      return Pool.Name;
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Pool : in out MOMA.Types.Message_Pool;
      Name :        MOMA.Types.String) is
   begin
      Pool.Name := Name;
   end Set_Name;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Pool : MOMA.Types.Message_Pool)
     return Pool_Type is
   begin
      return Pool.Pool;
   end Get_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Pool  : in out MOMA.Types.Message_Pool;
      PType :        Pool_Type) is
   begin
      Pool.Pool := PType;
   end Set_Type;

   --------------------
   -- Get_Persistent --
   --------------------

   function Get_Persistence
     (Pool : MOMA.Types.Message_Pool)
     return Persistence_Mode is
   begin
      return Pool.Persistence;
   end Get_Persistence;

   ---------------------
   -- Set_Persistence --
   ---------------------

   procedure Set_Persistence
     (Pool  : in out MOMA.Types.Message_Pool;
      PMode :        Persistence_Mode) is
   begin
      Pool.Persistence := PMode;
   end Set_Persistence;

   ------------------------
   -- To_Standard_String --
   ------------------------

   function To_Standard_String
     (V : MOMA.Types.String)
     return Standard.String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (V));
   end To_Standard_String;

   --------------------
   -- To_MOMA_String --
   --------------------

   function To_MOMA_String
     (V : Standard.String)
     return MOMA.Types.String
   is
   begin
      return Types.String
        (Ada.Strings.Unbounded.To_Unbounded_String (V));
   end To_MOMA_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use PolyORB.Utils.Strings;

   begin

      --  Map_Element

      TC_Map_Element := PolyORB.Any.TypeCode.TC_Struct;

      TypeCode.Add_Parameter
        (TC_Map_Element,
         PolyORB.Any.To_Any (To_PolyORB_String ("map_element")));

      TypeCode.Add_Parameter
        (TC_Map_Element,
         PolyORB.Any.To_Any (To_PolyORB_String
                             ("MOMA:types/map_element:1.0")));

      TypeCode.Add_Parameter
        (TC_Map_Element,
         PolyORB.Any.To_Any (TC_String));

      TypeCode.Add_Parameter
        (TC_Map_Element,
         PolyORB.Any.To_Any (To_PolyORB_String ("name")));

      TypeCode.Add_Parameter
        (TC_Map_Element,
         PolyORB.Any.To_Any (TC_Any));

      TypeCode.Add_Parameter
        (TC_Map_Element,
         PolyORB.Any.To_Any (To_PolyORB_String ("value")));

      --  IDL_SEQUENCE_Map_Element

      TC_IDL_SEQUENCE_Map_Element := PolyORB.Any.TypeCode.TC_Sequence;

      TypeCode.Add_Parameter
        (TC_IDL_SEQUENCE_Map_Element,
         PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (0)));

      TypeCode.Add_Parameter
        (TC_IDL_SEQUENCE_Map_Element,
         PolyORB.Any.To_Any (TC_Map_Element));

      --  Map

      TC_Map := PolyORB.Any.TypeCode.TC_Alias;

      TypeCode.Add_Parameter
        (TC_Map,
         PolyORB.Any.To_Any (To_PolyORB_String ("map")));

      TypeCode.Add_Parameter
        (TC_Map,
         PolyORB.Any.To_Any (To_PolyORB_String ("MOMA:types/map:1.0")));

      TypeCode.Add_Parameter
        (TC_Map,
         PolyORB.Any.To_Any (TC_IDL_SEQUENCE_Map_Element));

      --  Destination_Type

      TC_Destination_Type := PolyORB.Any.TypeCode.TC_Enum;

      declare
         Name           : constant String :=
                            To_PolyORB_String ("Destination_Type");
         Id             : constant String :=
                            To_PolyORB_String
                              ("MOMA:types/destination_type:1.0");
         Unknown_Name   : constant String := To_PolyORB_String ("Unknown");
         Pool_Name      : constant String := To_PolyORB_String ("Pool");
         Router_Name    : constant String := To_PolyORB_String ("Router");
         Topic_Name     : constant String := To_PolyORB_String ("Topic");
      begin
         TypeCode.Add_Parameter (TC_Destination_Type, To_Any (Name));
         TypeCode.Add_Parameter (TC_Destination_Type, To_Any (Id));
         TypeCode.Add_Parameter (TC_Destination_Type, To_Any (Unknown_Name));
         TypeCode.Add_Parameter (TC_Destination_Type, To_Any (Pool_Name));
         TypeCode.Add_Parameter (TC_Destination_Type, To_Any (Router_Name));
         TypeCode.Add_Parameter (TC_Destination_Type, To_Any (Topic_Name));
      end;

   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"MOMA.Types",
          Conflicts => Empty,
          Depends   => +"any",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end MOMA.Types;
