------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.CODE_SETS.CONVERTERS.UNICODE                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

--  This package provides character data code sets converters for
--  Unicode encoding: UTF-8 for char data and UTF-16 for wchar data.

--  Note: Client application which use these encodings should be ready
--  to dial with multibite character sequences and avoid use of char
--  IDL type (use string instead).

--  Supported char native code sets:
--    0x05010001  X/Open UTF-8; UCS Transformation Format 8 (UTF-8)
--
--  Supported char conversion code sets:
--    not provided
--
--  Supported wchar native code sets:
--    0x00010109  UTF-16, UCS Transformation Format 16-bit form
--
--  Supported wchar conversion code sets:
--    0x00010100  ISO/IEC 10646-1:1993; UCS-2, Level 1
--    0x00010101  ISO/IEC 10646-1:1993; UCS-2, Level 2
--    0x00010102  ISO/IEC 10646-1:1993; UCS-2, Level 3
--    0x00010104  ISO/IEC 10646-1:1993; UCS-4, Level 1
--    0x00010105  ISO/IEC 10646-1:1993; UCS-4, Level 2
--    0x00010106  ISO/IEC 10646-1:1993; UCS-4, Level 3

package PolyORB.GIOP_P.Code_Sets.Converters.Unicode is

   pragma Elaborate_Body;

end PolyORB.GIOP_P.Code_Sets.Converters.Unicode;
