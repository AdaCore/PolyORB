------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S N A M I N G . B I N D I N G I T E R A T O R . I M P L        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

with CORBA;
with PortableServer;

with PolyORB.Tasking.Mutexes;

package CosNaming.BindingIterator.Impl is

   type Object;
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
