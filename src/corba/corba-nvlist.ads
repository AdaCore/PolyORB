------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . N V L I S T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

pragma Ada_2012;

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
   overriding procedure Initialize (Self : in out Ref);

   pragma Inline (Add_Item);
   pragma Inline (Get_Count);
   pragma Inline (Free);

end CORBA.NVList;
