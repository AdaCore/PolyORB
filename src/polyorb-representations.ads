------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R E P R E S E N T A T I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Data representation methods.

--  A Representation is a method for transforming an arbitrary piece
--  of data (in the form of an 'Any' object) into a sequence of
--  Stream_Elements, and back.

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Errors;

package PolyORB.Representations is

   type Representation is abstract tagged limited private;

   type Representation_Access is access all Representation;

   procedure Marshall_From_Any
     (R      : Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container) is abstract;
   --  Store a representation of Data into Buffer according to representation
   --  convention R.

   procedure Unmarshall_To_Any
     (R      : Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container) is abstract;
   --  Set the value of Data from the representation stored in Buffer according
   --  to representation convetion R.

   procedure Release (R : in out Representation);
   --  Deallocate resources associated with the given representation engine

private

   type Representation is abstract tagged limited null record;

end PolyORB.Representations;
