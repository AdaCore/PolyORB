------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S N A M I N G . B I N D I N G I T E R A T O R . I M P L        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2020, Free Software Foundation, Inc.          --
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

with CORBA;
with PortableServer;

with PolyORB.Tasking.Mutexes;

package CosNaming.BindingIterator.Impl is

   type Object is tagged;
   type Object_Ptr is access all Object'Class;

   package Bindings renames IDL_SEQUENCE_CosNaming_Binding;
   package PTM renames PolyORB.Tasking.Mutexes;

   type Binding_Element_Array_Ptr is access Bindings.Element_Array;

   type Object is new PortableServer.Servant_Base with record
      Self  : Object_Ptr;
      Index : Natural;
      Table : Binding_Element_Array_Ptr;
      Mutex : PTM.Mutex_Access;
   end record;

   procedure Next_One
     (Self    : access Object;
      B       : out CosNaming.Binding;
      Returns : out CORBA.Boolean);

   procedure Next_N
     (Self     : access Object;
      How_Many : CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      Returns  : out CORBA.Boolean);

   procedure Destroy (Self : access Object);

   function Create return Object_Ptr;

end CosNaming.BindingIterator.Impl;
