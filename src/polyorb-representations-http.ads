------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . H T T P         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  A data representation used for implementing the HTTP protocol.
--  HTTP is standardised by IETF RFC2616:
--  Hypertext Transfer Protocol -- HTTP/1.1.
--  R. Fielding, J. Gettys, J. Mogul, H. Frystyk, L. Masinter,
--  P. Leach, T. Berners-Lee. June 1999.

--  $Id$

with Ada.Streams;

package  PolyORB.Representations.HTTP is

   pragma Elaborate_Body;

   function Decode_URL (Str : in String) return String;
   --  The translations are:
   --     +     should be changed to a space
   --     %xy   should be replaced by the character whose code is xy

   function Encode_Stream (Data : Ada.Streams.Stream_Element_Array)  return String;
   --  Encode Data using the base64 algorithm

   function Encode_String (Data : in String) return String;
   --  Encode Data using the base64 algorithm also but it takes a string as input

   function Decode (B64_Data : in String) return Ada.Streams.Stream_Element_Array;
   --  Decode Data using the base64 algorithm

end  PolyORB.Representations.HTTP;
