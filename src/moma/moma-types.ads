------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           M O M A . T Y P E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  MOMA Types definitions.

--  $Id$

with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Sequences.Unbounded;

package MOMA.Types is

   use PolyORB.Any;
   use PolyORB.Types;

   --
   --  Generic types.
   --

   subtype Boolean        is PolyORB.Types.Boolean;
   subtype Byte           is PolyORB.Types.Octet;
   subtype Char           is PolyORB.Types.Char;
   subtype Double         is PolyORB.Types.Double;
   subtype Float          is PolyORB.Types.Float;
   subtype Long           is PolyORB.Types.Long;
   subtype Short          is PolyORB.Types.Short;
   subtype String         is PolyORB.Types.String;
   subtype Unsigned_Long  is PolyORB.Types.Unsigned_Long;
   subtype Unsigned_Short is PolyORB.Types.Unsigned_Short;

   --
   --  String conversion fonctions.
   --

   function To_Standard_String (V : PolyORB.Types.String)
                                return Standard.String
     renames PolyORB.Types.To_Standard_String;

   function To_MOMA_String (V : Standard.String)
                            return PolyORB.Types.String
     renames PolyORB.Types.To_PolyORB_String;

   --
   --  MOMA specific types.
   --

   --  'Map' type, see JMS spec. for more details.
   --  As implemented, a 'Map' is an unbounded sequence of
   --  'Map_Elements'.
   --
   --  Note that 'Map' type inherits all primitives from
   --  the PolyORB.Sequences.Unbounded package.

   --  Map_Element type.

   type Map_Element is record
     Name : MOMA.Types.String;
     Value : PolyORB.Any.Any;
   end record;

   TC_Map_Element : TypeCode.Object := TypeCode.TC_Struct;

   function To_Any (Item : in Map_Element)
     return PolyORB.Any.Any;

   function From_Any (Item : in PolyORB.Any.Any)
     return Map_Element;

   --  function "=" (L, R : Map_Element)
   --    return Boolean;

   function Get_Boolean (Self : Map_Element)
                         return MOMA.Types.Boolean;

   procedure Set_Boolean (Self : in out Map_Element;
                          Value : MOMA.Types.Boolean);

   function Get_Byte (Self : Map_Element)
                      return MOMA.Types.Byte;

   procedure Set_Byte (Self : in out Map_Element;
                       Value : MOMA.Types.Byte);

   function Get_Char (Self : Map_Element)
                      return MOMA.Types.Char;

   procedure Set_Char (Self : in out Map_Element;
                       Value : MOMA.Types.Char);

   function Get_Double (Self : Map_Element)
                        return MOMA.Types.Double;

   procedure Set_Double (Self : in out Map_Element;
                         Value : MOMA.Types.Double);

   function Get_Float (Self : Map_Element)
                       return MOMA.Types.Float;

   procedure Set_Float (Self : in out Map_Element;
                        Value : MOMA.Types.Float);

   function Get_Long (Self : Map_Element)
                      return MOMA.Types.Long;

   procedure Set_Long (Self : in out Map_Element;
                       Value : MOMA.Types.Long);

   function Get_Name (Self : Map_Element)
                      return MOMA.Types.String;

   procedure Set_Name (Self : in out Map_Element;
                       Value : MOMA.Types.String);

   function Get_Short (Self : Map_Element)
                       return MOMA.Types.Short;

   procedure Set_Short (Self : in out Map_Element;
                        Value : MOMA.Types.Short);

   function Get_String (Self : Map_Element)
                        return MOMA.Types.String;

   procedure Set_String (Self : in out Map_Element;
                         Value : MOMA.Types.String);

   function Get_Unsigned_Short (Self : Map_Element)
                                return MOMA.Types.Unsigned_Short;

   procedure Set_Unsigned_Short (Self : in out Map_Element;
                                 Value : MOMA.Types.Unsigned_Short);

   function Get_Unsigned_Long (Self : Map_Element)
                               return MOMA.Types.Unsigned_Long;

   procedure Set_Unsigned_Long (Self : in out Map_Element;
                                Value : MOMA.Types.Unsigned_Long);

   --  Map type.

   package IDL_SEQUENCE_Map_Element is
     new PolyORB.Sequences.Unbounded (Map_Element);

   TC_IDL_SEQUENCE_Map_Element : TypeCode.Object
     := TypeCode.TC_Sequence;

   function From_Any (Item : in PolyORB.Any.Any)
      return IDL_SEQUENCE_Map_Element.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_Map_Element.Sequence)
     return PolyORB.Any.Any;

   TC_Map : TypeCode.Object := TypeCode.TC_Alias;

   type Map is new MOMA.Types.IDL_SEQUENCE_Map_Element.Sequence;

   function To_Any (Item : in Map)
     return PolyORB.Any.Any;

   function From_Any (Item : in PolyORB.Any.Any)
     return Map;

   --
   --  MOMA administrative types.
   --

   type Pool_Type is (Queue,
                      Topic);

   type Persistence_Mode is (None,
                             File);

   type Message_Type is (Any_M,
                         Byte_M,
                         Map_M,
                         Text_M);

   type Message_Pool is record
      Pool        : MOMA.Types.Pool_Type;
      Name        : MOMA.Types.String;
      Persistence : MOMA.Types.Persistence_Mode;
   end record;
   --  XXX should be private !

   type Meta_Data        is new    Integer;
   type Acknowledge_Type is new    Integer;
   type Property_Type    is new    Integer;
   type Priority         is new    Integer range 1 .. 10;
   --  XXX to be completed

end MOMA.Types;

