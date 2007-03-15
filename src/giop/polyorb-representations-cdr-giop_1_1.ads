------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_1                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  Support package for CDR representation of char and strings for GIOP 1.1

with PolyORB.GIOP_P.Code_Sets.Converters;

package PolyORB.Representations.CDR.GIOP_1_1 is

   pragma Elaborate_Body;

   type GIOP_1_1_CDR_Representation is new CDR_Representation with private;

   type GIOP_1_1_CDR_Representation_Access is
      access all GIOP_1_1_CDR_Representation;

   --  XXX Encapsulation is also GIOP version dependent.

   procedure Set_Converters
     (R : in out GIOP_1_1_CDR_Representation;
      C : PolyORB.GIOP_P.Code_Sets.Converters.Converter_Access;
      W : PolyORB.GIOP_P.Code_Sets.Converters.Wide_Converter_Access);
   --  Set code sets converters for Character/String and
   --  Wide_Character/Wide_String types. Code set converters may be
   --  null value. If the code set converter is set to null, then we
   --  assume what the backward compatibility mode with GIOP 1.0
   --  enabled.

   procedure Release (R : in out GIOP_1_1_CDR_Representation);
   --  Deallocate content of R

private

   type GIOP_1_1_CDR_Representation is new CDR_Representation with record
      C_Converter : PolyORB.GIOP_P.Code_Sets.Converters.Converter_Access;
      W_Converter : PolyORB.GIOP_P.Code_Sets.Converters.Wide_Converter_Access;
   end record;

   --  'char' type

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Char;
      Error  : in out Errors.Error_Container);

   --  'wchar' type

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container);

   --  'string' type

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.String;
      Error  : in out Errors.Error_Container);

   --  'wstring' type

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container);

end PolyORB.Representations.CDR.GIOP_1_1;
