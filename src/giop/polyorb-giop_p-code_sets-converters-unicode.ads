------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.CODE_SETS.CONVERTERS.UNICODE                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

--  This package provides character data code sets converters for Unicode
--  encodings: UTF-8 for char data and UTF-16 for wchar data.

--  Note: Client application which use these encodings should be ready to deal
--  with multibyte character sequences and avoid use of char IDL type (use
--  string instead).

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
