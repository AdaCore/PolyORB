------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . U T I L S . H T T P                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  $Id$

with Ada.Streams; use Ada.Streams;
--  with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

--  with PolyORB.Types; use PolyORB.Types;
--  with PolyORB.Sockets;
--  with PolyORB.Buffers; use PolyORB.Buffers;
--  with PolyORB.Transport; use PolyORB.Transport;

--  with PolyORB.Utils.HTTP_Messages;

with Ada.Streams;


package PolyORB.Utils.HTTP is


   ---  Utilities functions

   function Base64_Encode (Data : Stream_Element_Array)
                          return String;

   function Base64_Encode (Data : in String) return String;

   function Base64_Decode (B64_Data : in String)
                          return Stream_Element_Array;


end  PolyORB.Utils.HTTP;
