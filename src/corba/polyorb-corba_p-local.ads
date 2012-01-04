------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O R B A _ P . L O C A L                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.Object;

with PolyORB.Smart_Pointers;
with PolyORB.Smart_Pointers.Controlled_Entities;

package PolyORB.CORBA_P.Local is

   type Local_Object_Base is
      abstract new PolyORB.Smart_Pointers.Controlled_Entities.Entity
      with null record;

   type Local_Object_Base_Ref is access all Local_Object_Base'Class;

   function Is_A
     (Obj             : not null access Local_Object_Base;
      Logical_Type_Id : String) return Boolean is abstract;

   function Is_Local (Self : CORBA.Object.Ref'Class) return Boolean;
   --  True iff Self is a valid reference a local object.
   --  Raise CORBA.Inv_Objref if reference is null.

   function Is_CORBA_Local (Self : CORBA.Object.Ref'Class) return Boolean;
   --  True iff Self is a valid reference a CORBA personality local object.
   --  Raise CORBA.Inv_Objref if reference is null.

end PolyORB.CORBA_P.Local;
