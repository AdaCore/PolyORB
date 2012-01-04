------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . R E P R E S E N T A T I O N S . T E S T          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  A dummy data representation method, just for show.

with PolyORB.Buffers;

package PolyORB.Representations.Test is

   pragma Elaborate_Body;

   use PolyORB.Buffers;

   type Rep_Test is new Representation with private;
   type Rep_Test_Access is access all Rep_Test;

   --  A real representation function should implement the
   --  following two subprograms.

   procedure Marshall_From_Any
     (R      : access Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall_To_Any
     (R      : access Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   --  The following methods are specific to Rep_Test and are
   --  here only to facilitate testing of other parts of the ORB.

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character);
   --  Marshall one character.

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character;
   --  Unmarshall one character.

   procedure Marshall_String
     (R : Rep_Test;
      B : access Buffer_Type;
      S : String);
   --  Marshall a string.

   function Unmarshall_String
     (R : Rep_Test;
      B : access Buffer_Type)
     return String;
   --  Unmarshall a string terminated by a CR/LF sequence.

private

   type Rep_Test is new Representation with null record;

end PolyORB.Representations.Test;
