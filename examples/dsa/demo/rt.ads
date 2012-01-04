------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                   R T                                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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
