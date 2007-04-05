------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  A data representation implementing the CORBA Common Data Representation.
--  For reference on CDR see:
--    The Common Object Request Broker Architecture: Core Specification,
--    Version 3.0", Open Management Group
--    (http://www.omg.org/).

with PolyORB.Types;
with PolyORB.Utils.Dynamic_Tables;

package PolyORB.Representations.CDR is

--   pragma Elaborate_Body;

   type CDR_Representation is abstract new Representation with private;
   type CDR_Representation_Access is access all CDR_Representation'Class;

   --  The next two subprograms marshall or unmarshall the value of
   --  the Any, not the Any type itself (i.e. they do not marshall Data's
   --  typecode).

   procedure Marshall_From_Any
     (R      : access CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      CData  : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall_To_Any
     (R      : access CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      CData  : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   --  XXX Encapsulation is also GIOP version dependent.

   --  'char' type

   procedure Marshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Char;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Char;
      Error  : in out Errors.Error_Container)
      is abstract;

   --  'wchar' type

   procedure Marshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container)
      is abstract;

   --  'string' type

   procedure Marshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.String;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.String;
      Error  : in out Errors.Error_Container)
      is abstract;

   --  'wstring' type

   procedure Marshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container)
      is abstract;

   function Create_Representation
     (Major : Types.Octet;
      Minor : Types.Octet) return CDR_Representation_Access;
   --  Create Representation object for requested version

   procedure Release (Representation : in out CDR_Representation);

   --  'Any' type

   procedure Marshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : access CDR_Representation'Class;
      Data           : PolyORB.Any.Any);

   function Unmarshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : access CDR_Representation'Class) return PolyORB.Any.Any;

private

   --  Typecodes map management
   --  When a complex typecode is marshalled into a CDR stream, nested
   --  typecodes can be stored as indirect references to a previous occurrence
   --  of the same typecode within the same enclosing outermost complex
   --  typecode. This is supported by keeping track of the mapping between
   --  typecodes and their offset in the CDR stream within each CDR engine.

   type TC_Map_Entry is record
      Enclosing_Complex : Types.Long;
      --  Index in the TC_Map of the innermost enclosing complex typecode, used
      --  for computation of offset relative the to topmost complex typecode.
      --  Set to -1 for the outermost complex TC.

      TC_Ref            : Any.TypeCode.Object;
      --  TC object at this offset. Assumes reference semantics

      Offset            : Types.Long;
      --  Offset of this typecode in outermost CDR stream
   end record;

   package TC_Maps is new PolyORB.Utils.Dynamic_Tables
     (Table_Component_Type => TC_Map_Entry,
      Table_Index_Type     => Types.Long,
      Table_Low_Bound      => 0,
      Table_Initial        => 1,
      Table_Increment      => 4);

   use type Types.Long;
   --  For unary minus operator used for component Current_Complex below

   type CDR_Representation is abstract new Representation with record
      TC_Map : TC_Maps.Instance;
      --  Map of typecodes in current CDR stream. This map is flushed when the
      --  outermost complex typecode has been completely processed.

      Current_Complex : Types.Long := -1;
      --  Index in TC_Map of complex typecode currently being processed, or
      --  -1 if none.
   end record;

   --  'TypeCode.Object' type

   procedure Marshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : access CDR_Representation'Class;
      Data           : PolyORB.Any.TypeCode.Object);

   function Unmarshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : access CDR_Representation'Class)
      return PolyORB.Any.TypeCode.Object;

   --  CDR Representation versions registry

   type CDR_Representation_Factory is
      access function return CDR_Representation_Access;

   procedure Register_Factory
     (Major   : Types.Octet;
      Minor   : Types.Octet;
      Factory : CDR_Representation_Factory);

end PolyORB.Representations.CDR;
