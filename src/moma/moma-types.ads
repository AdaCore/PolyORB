------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           M O M A . T Y P E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  This package provides definition of all MOMA types.

with Ada.Strings.Unbounded;

with PolyORB.Any;

with PolyORB.References;
with PolyORB.Sequences.Unbounded;
with PolyORB.Tasking.Priorities;
with PolyORB.Types;

package MOMA.Types is

   use PolyORB.Types;

   -----------------
   -- Basic types --
   -----------------

   subtype Any            is PolyORB.Any.Any;
   type Boolean        is new PolyORB.Types.Boolean;
   type Byte           is new PolyORB.Types.Octet;
   type Char           is new PolyORB.Types.Char;
   type Double         is new PolyORB.Types.Double;
   type Float          is new PolyORB.Types.Float;
   type Long           is new PolyORB.Types.Long;
   type Short          is new PolyORB.Types.Short;
   type String         is new PolyORB.Types.String;
   type Unsigned_Long  is new PolyORB.Types.Unsigned_Long;
   type Unsigned_Short is new PolyORB.Types.Unsigned_Short;
   subtype Ref            is PolyORB.References.Ref;

   function To_Any (Item : Any)                return Any;
   function To_Any (Item : Boolean)            return Any;
   function To_Any (Item : Byte)               return Any;
   function To_Any (Item : Char)               return Any;
   function To_Any (Item : Double)             return Any;
   function To_Any (Item : MOMA.Types.Float)   return Any;
   function To_Any (Item : Long)               return Any;
   function To_Any (Item : Short)              return Any;
   function To_Any (Item : MOMA.Types.String)  return Any;
   function To_Any (Item : Unsigned_Short)     return Any;
   function To_Any (Item : Unsigned_Long)      return Any;

   function From_Any (Item : Any) return Any;
   function From_Any (Item : Any) return Boolean;
   function From_Any (Item : Any) return Byte;
   function From_Any (Item : Any) return Char;
   function From_Any (Item : Any) return Double;
   function From_Any (Item : Any) return MOMA.Types.Float;
   function From_Any (Item : Any) return Long;
   function From_Any (Item : Any) return Short;
   function From_Any (Item : Any) return MOMA.Types.String;
   function From_Any (Item : Any) return Unsigned_Long;
   function From_Any (Item : Any) return Unsigned_Short;

   function "=" (Left, Right : Any) return Standard.Boolean
     renames PolyORB.Any."=";

   function "=" (Left, Right : Ref) return Standard.Boolean
     renames PolyORB.References."=";

   Nil_Ref : constant MOMA.Types.Ref
     := MOMA.Types.Ref (PolyORB.References.Nil_Ref);

   ---------------------------------
   -- String conversion fonctions --
   ---------------------------------

   function To_Standard_String (V : MOMA.Types.String) return Standard.String;

   function To_MOMA_String (V : Standard.String) return MOMA.Types.String;

   -------------------------
   -- MOMA specific types --
   -------------------------

   --  The Map type, see JMS spec. for more details.  As implemented,
   --  a Map is an unbounded sequence of Map_Elements.
   --
   --  Note that Map type inherits all primitives from the
   --  PolyORB.Sequences.Unbounded package.

   --  Map_Element type

   type Map_Element is record
     Name  : MOMA.Types.String;
     Value : MOMA.Types.Any;
   end record;

   TC_Map_Element : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Struct;

   function To_Any (Item : Map_Element) return MOMA.Types.Any;
   function From_Any (Item : MOMA.Types.Any) return Map_Element;

   function Get_Boolean
     (Self : Map_Element)
     return MOMA.Types.Boolean;

   procedure Set_Boolean
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Boolean);

   function Get_Byte
     (Self : Map_Element)
     return MOMA.Types.Byte;

   procedure Set_Byte
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Byte);

   function Get_Char
     (Self : Map_Element)
     return MOMA.Types.Char;

   procedure Set_Char
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Char);

   function Get_Double
     (Self : Map_Element)
     return MOMA.Types.Double;

   procedure Set_Double
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Double);

   function Get_Float
     (Self : Map_Element)
     return MOMA.Types.Float;

   procedure Set_Float
     (Self  : in out Map_Element;
      Value : MOMA.Types.Float);

   function Get_Long
     (Self : Map_Element)
     return MOMA.Types.Long;

   procedure Set_Long
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Long);

   function Get_Name
     (Self : Map_Element)
     return MOMA.Types.String;

   procedure Set_Name
     (Self  : in out Map_Element;
      Value :        MOMA.Types.String);

   function Get_Short
     (Self : Map_Element)
     return MOMA.Types.Short;

   procedure Set_Short
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Short);

   function Get_String
     (Self : Map_Element)
     return MOMA.Types.String;

   procedure Set_String
     (Self  : in out Map_Element;
      Value :        MOMA.Types.String);

   function Get_Unsigned_Short
     (Self : Map_Element)
     return MOMA.Types.Unsigned_Short;

   procedure Set_Unsigned_Short
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Unsigned_Short);

   function Get_Unsigned_Long
     (Self : Map_Element)
     return MOMA.Types.Unsigned_Long;

   procedure Set_Unsigned_Long
     (Self  : in out Map_Element;
      Value :        MOMA.Types.Unsigned_Long);

   --  Map type

   package IDL_SEQUENCE_Map_Element is
     new PolyORB.Sequences.Unbounded (Map_Element);

   TC_IDL_SEQUENCE_Map_Element : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_Map_Element.Sequence)
     return MOMA.Types.Any;

   function From_Any
     (Item : MOMA.Types.Any)
      return IDL_SEQUENCE_Map_Element.Sequence;

   TC_Map : PolyORB.Any.TypeCode.Object := PolyORB.Any.TypeCode.TC_Alias;

   type Map is new MOMA.Types.IDL_SEQUENCE_Map_Element.Sequence;

   function To_Any (Item : Map) return MOMA.Types.Any;
   function From_Any (Item : MOMA.Types.Any) return Map;

   -------------------------------
   -- MOMA administrative types --
   -------------------------------

   MOMA_Type_Id : constant MOMA.Types.String;

   --  Destination type

   type Destination_Type is (Unknown, Pool, Router, Topic);

   TC_Destination_Type : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Enum;

   function To_Any (Item : Destination_Type) return MOMA.Types.Any;
   function From_Any (Item : MOMA.Types.Any) return Destination_Type;

   --  Pool type

   type Pool_Type is (Queue, Topic);

   --  Persistence mode

   type Persistence_Mode is (None, File);

   --  Message type

   type Message_Type is (Any_M, Byte_M, Execute_M, Map_M, Text_M);

   --  Callback

   type Call_Back_Behavior is (Notify, Handle, None);
   --  Behaviors for call-back : only notify message presence, send
   --  the message when received for Handle, or None.

   --  Message_Pool type

   type Message_Pool is private;
   --  Type        : type of the pool
   --  Name        : name of the pool
   --  Persistence : persistence mode of the pool

   function Get_Name
     (Pool : MOMA.Types.Message_Pool)
     return MOMA.Types.String;

   procedure Set_Name
     (Pool : in out MOMA.Types.Message_Pool;
      Name :        MOMA.Types.String);

   function Get_Type
     (Pool : MOMA.Types.Message_Pool)
     return MOMA.Types.Pool_Type;

   procedure Set_Type
     (Pool  : in out MOMA.Types.Message_Pool;
      PType :        Pool_Type);

   function Get_Persistence
     (Pool : MOMA.Types.Message_Pool)
     return MOMA.Types.Persistence_Mode;

   procedure Set_Persistence
     (Pool  : in out MOMA.Types.Message_Pool;
      PMode :        Persistence_Mode);

   --  Priority

   subtype Priority is PolyORB.Tasking.Priorities.External_Priority;

   Invalid_Priority : constant Priority;

   --  Other types
   --  XXX to be clarified

   type Meta_Data        is new Integer;
   type Acknowledge_Type is new Integer;
   type Property_Type    is new Integer;

private

   MOMA_Type_Id : constant MOMA.Types.String := MOMA.Types.String
     (Ada.Strings.Unbounded.To_Unbounded_String ("MOMA"));

   type Message_Pool is record
      Pool        : MOMA.Types.Pool_Type;
      Name        : MOMA.Types.String;
      Persistence : MOMA.Types.Persistence_Mode;
   end record;

   Invalid_Priority : constant Priority :=
     Priority (PolyORB.Tasking.Priorities.Invalid_Priority);

end MOMA.Types;
