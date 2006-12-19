------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . N V L I S T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.AbstractBase;
with PolyORB.Any.NVList;

package CORBA.NVList is

   type Ref is new CORBA.AbstractBase.Ref with private;

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : Identifier;
      Item       : CORBA.Any;
      Item_Flags : Flags);
   --  Create a NamedValue and add it to this NVList

   procedure Add_Item
     (Self : Ref;
      Item : CORBA.NamedValue);
   --  Add a NamedValue to this NVList

   function Get_Count (Self : Ref) return CORBA.Long;
   --  Return the number of items in this NVList

   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref) renames Free;
   --  Implementation Note: As per the IDL-to-Ada mapping, Free and
   --  Free_Memory are no-ops.

   package Internals is

      --  Internal implementation subprograms. These shall not be used outside
      --  of PolyORB.

      function Item (Self : Ref; Index : CORBA.Long) return CORBA.NamedValue;

      function To_PolyORB_Ref (Self : Ref) return PolyORB.Any.NVList.Ref;
      function To_CORBA_Ref (Self : PolyORB.Any.NVList.Ref) return Ref;
      pragma Inline (To_PolyORB_Ref);
      pragma Inline (To_CORBA_Ref);

      procedure Clone_Out_Args (Self : Ref);
      --  For any NV in Self that has mode out or in out, replace the Argument
      --  component with a by-value copy of the original one (thus ensuring
      --  that the value remains valid even after exiting the current scope).

   end Internals;

private

   type Ref is new CORBA.AbstractBase.Ref with null record;
   procedure Initialize (Self : in out Ref);

   pragma Inline (Add_Item);
   pragma Inline (Get_Count);
   pragma Inline (Free);

end CORBA.NVList;
