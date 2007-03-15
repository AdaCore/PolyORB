------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
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

--  A data representation implementing the CORBA Common Data Representation.
--  For reference on CDR see:
--    The Common Object Request Broker Architecture: Core Specification,
--    Version 3.0", Open Management Group
--    (http://www.omg.org/).

with PolyORB.Types;

package PolyORB.Representations.CDR is

--   pragma Elaborate_Body;

   type CDR_Representation is abstract new Representation with null record;

   type CDR_Representation_Access is access all CDR_Representation'Class;

   --  The next two subprograms marshall or unmarshall the value of
   --  the Any, not the Any type itself (i.e. they do not marshall Data's
   --  typecode).

   procedure Marshall_From_Any
     (R      : CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      CData  : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall_To_Any
     (R      : CDR_Representation;
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
      Minor : Types.Octet)
      return CDR_Representation_Access;
   --  Create Representation object for requested version

   --  'Any' type

   procedure Marshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : CDR_Representation'Class;
      Data           : PolyORB.Any.Any);

   function Unmarshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : CDR_Representation'Class)
      return PolyORB.Any.Any;

private

   --  'TypeCode.Object' type

   procedure Marshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : CDR_Representation'Class;
      Data           : PolyORB.Any.TypeCode.Object);

   function Unmarshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : CDR_Representation'Class)
      return PolyORB.Any.TypeCode.Object;

   --  CDR Representation versions registry

   type CDR_Representation_Factory is
      access function return CDR_Representation_Access;

   procedure Register_Factory
     (Major   : Types.Octet;
      Minor   : Types.Octet;
      Factory : CDR_Representation_Factory);

end PolyORB.Representations.CDR;
