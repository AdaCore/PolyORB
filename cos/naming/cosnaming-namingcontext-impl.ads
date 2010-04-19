------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         C O S N A M I N G . N A M I N G C O N T E X T . I M P L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

package CosNaming.NamingContext.Impl is

   type Object is new PortableServer.Servant_Base with private;
   type Object_Ptr is access all Object'Class;

   procedure Bind
     (Self : access Object;
      N    : CosNaming.Name;
      Obj  : CORBA.Object.Ref);

   procedure Rebind
     (Self : access Object;
      N    : CosNaming.Name;
      Obj  : CORBA.Object.Ref);

   procedure Bind_Context
     (Self : access Object;
      N    : CosNaming.Name;
      NC   : CosNaming.NamingContext.Ref);

   procedure Rebind_Context
     (Self : access Object;
      N    : CosNaming.Name;
      NC   : CosNaming.NamingContext.Ref);

   function Resolve
     (Self : access Object;
      N    : CosNaming.Name)
     return CORBA.Object.Ref;

   procedure Unbind
     (Self : access Object;
      N    : CosNaming.Name);

   function New_Context
     (Self : access Object)
     return CosNaming.NamingContext.Ref'Class;

   function Bind_New_Context
     (Self : access Object;
      N    : CosNaming.Name)
     return CosNaming.NamingContext.Ref'Class;

   procedure Destroy (Self : access Object);

   procedure List
     (Self     : access Object;
      How_Many : CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      BI       : out CosNaming.BindingIterator_Forward.Ref);

   function Create return CosNaming.NamingContext.Impl.Object_Ptr;

   procedure Initialize (Self : Object_Ptr);

private
   package PTM renames PolyORB.Tasking.Mutexes;

   Key_Size : constant := 4;
   type Key_Type is new String (1 .. Key_Size);

   type Bound_Object;
   type Bound_Object_Ptr is access Bound_Object;

   type Bound_Object is record
      BN   : NameComponent;
      BT   : BindingType;
      Obj  : CORBA.Object.Ref;
      Prev : Bound_Object_Ptr;
      Next : Bound_Object_Ptr;
      NC   : Object_Ptr;
   end record;

   type Object is new PortableServer.Servant_Base with record
      Key  : Key_Type;
      Self : Object_Ptr;
      Prev : Object_Ptr;
      Next : Object_Ptr;
      Head : Bound_Object_Ptr;
      Tail : Bound_Object_Ptr;
      Mutex : PTM.Mutex_Access;
   end record;

end CosNaming.NamingContext.Impl;
