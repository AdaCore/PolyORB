------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     S Y S T E M . D S A _ T Y P E S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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
package System.DSA_Types is
   pragma Remote_Types;
   type Any_Container_Ptr is private;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Any_Container_Ptr);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Any_Container_Ptr);

   for Any_Container_Ptr'Read use Read;
   for Any_Container_Ptr'Write use Write;

private
   type Dummy_Any_Container is abstract tagged limited null record;

   type Any_Container_Ptr is access all Dummy_Any_Container'Class;
   for Any_Container_Ptr'Storage_Size use 0;

   --  This access type must never be derefenced, it is meant to be
   --  unchecked-converted to PolyORB.Any.Any_Container_Ptr.

end System.DSA_Types;
