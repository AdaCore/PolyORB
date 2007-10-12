------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            A W S . U T I L S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with System;

with MD5;

package AWS.Utils is

   type Random_Integer is range 0 ..
     Long_Long_Integer'Min (2 ** 48 - 1, System.Max_Int);
   --  Integer type for random number generation. As GNAT warns us
   --  about the reliability of random generator for numbers > 2^48,
   --  we must stay below this limit.

   --  General helper functions are to be found here.

   function Random return Random_Integer;
   --  Returns a random integer number.

   function Image (N : Natural) return String;
   --  return image of N without the leading blank.

   function Image (D : Duration) return String;
   --  return image of N without the leading blank and with only 2 decimals
   --  numbers.

   function Hex (V : Natural; Width : Natural := 0) return String;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : String) return Natural;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   function Is_Number (S : String) return Boolean;
   --  Returns True is S contains only decimal digits and is not empty.

   function Get_MD5 (Data : String) return MD5.Digest_String;
   --  Returns the MD5 digest value for the Data string.

   function Quote (Str : String) return String;
   pragma Inline (Quote);
   --  Returns Str with character '"' added at the start and the end.

   function CRLF_2_Spaces (Str : String) return String;
   --  Returns an str in a single line. All CR and LF are converted to spaces,
   --  trailing spaces are removed.

   ---------------
   -- Semaphore --
   ---------------

   --  This is a binary semaphore, only a single task can enter it (Seize) and
   --  must call Release when the resource is not needed anymore. This
   --  implement a standard semaphore (P/V mutex).

   protected type Semaphore is
      entry Seize;
      procedure Release;
   private
      Seized : Boolean := False;
   end Semaphore;

   ------------------
   -- RW_Semaphore --
   ------------------

   --  This is a Read/Write semaphore. Many reader tasks can enter (Read) at
   --  the same time excluding all writers (Write). A single writer can enter
   --  (Write) excluding all readers (Read). The task must release the
   --  corresponding resource by calling either Release_Read or Release_Write.
   --  As soon as a writer arrive all readers will wait for it to complete.
   --  Writers discriminant is the maximum number of writers accepted into the
   --  critical section.

   protected type RW_Semaphore (Writers : Positive) is

      --  Readers must call Read to enter the critical section and call
      --  Release_Read at the end.

      entry Read;

      procedure Release_Read;

      --  Writers must call Write to enter the critical section and call
      --  Release_Write at the end.

      entry Write;

      procedure Release_Write;

   private
      R, W : Natural := 0;
   end RW_Semaphore;

   -------------
   -- Streams --
   -------------

   type Stream_Element_Array_Access is access Ada.Streams.Stream_Element_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Ada.Streams.Stream_Element_Array, Stream_Element_Array_Access);

end AWS.Utils;
