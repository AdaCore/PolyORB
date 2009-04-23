------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                   R T                                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

with Ada.Streams; use Ada.Streams;

package RT is
   pragma Remote_Types;

   type Obj is abstract tagged limited private;
   procedure Method (Self : Obj) is abstract;
   procedure Method2 (Self : Obj; N : Integer) is abstract;
   procedure Method3 (Self : Obj; Other : Obj) is abstract;
   function Tekitoa (Self : Obj) return String is abstract;
   type RACW is access all Obj'Class;

   type Limited_Data is limited private;
   procedure Read (S : access Root_Stream_Type'Class; V : out Limited_Data);
   procedure Write (S : access Root_Stream_Type'Class; V : Limited_Data);
   for Limited_Data'Read use Read;
   for Limited_Data'Write use Write;

   procedure Show (Name : String; X : Limited_Data);

private
   type Obj is abstract tagged limited null record;

   type Limited_Data is limited record
      Value : Integer := 0;
   end record;

   for Limited_Data'Size use Integer'Size;
   for Limited_Data use record
      Value at 0 range 0 .. Integer'Size - 1;
   end record;
end RT;
