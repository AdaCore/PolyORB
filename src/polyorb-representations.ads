------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R E P R E S E N T A T I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

--  Data representation methods.

--  A Representation is a method for transforming an arbitrary piece
--  of data (in the form of an 'Any' object) into a sequence of
--  Stream_Elements, and back.

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Errors;

package PolyORB.Representations is

   pragma Preelaborate;

   type Representation is abstract tagged limited private;
   type Representation_Access is access all Representation'Class;

   procedure Marshall_From_Any
     (R      : access Representation;
      Buffer : not null access Buffers.Buffer_Type;
      Data   : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container) is abstract;
   --  Store a representation of Data into Buffer according to representation
   --  convention R.

   procedure Unmarshall_To_Any
     (R      : access Representation;
      Buffer : not null access Buffers.Buffer_Type;
      Data   : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container) is abstract;
   --  Set the value of Data from the representation stored in Buffer according
   --  to representation convetion R.

   procedure Release (R : in out Representation);
   --  Deallocate resources associated with the given representation engine

private

   type Representation is abstract tagged limited null record;

end PolyORB.Representations;
