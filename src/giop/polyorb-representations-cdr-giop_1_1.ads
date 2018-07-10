------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_1                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

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

   overriding procedure Release (R : in out GIOP_1_1_CDR_Representation);
   --  Deallocate content of R

private

   type GIOP_1_1_CDR_Representation is new CDR_Representation with record
      C_Converter : PolyORB.GIOP_P.Code_Sets.Converters.Converter_Access;
      W_Converter : PolyORB.GIOP_P.Code_Sets.Converters.Wide_Converter_Access;
   end record;

   --  'char' type

   overriding procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Char;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Char;
      Error  : in out Errors.Error_Container);

   --  'wchar' type

   overriding procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container);

   --  'string' type

   overriding procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.String;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.String;
      Error  : in out Errors.Error_Container);

   --  'wstring' type

   overriding procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container);

end PolyORB.Representations.CDR.GIOP_1_1;
