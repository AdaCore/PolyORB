------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         C O S N A M I N G . N A M I N G C O N T E X T . I M P L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2014, Free Software Foundation, Inc.          --
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

private with PolyORB.Tasking.Mutexes;
private with PolyORB.Utils.Strings;

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
   use PolyORB.Utils.Strings;

   package PTM renames PolyORB.Tasking.Mutexes;

   Key_Size : constant := 4;
   type Key_Type is new String (1 .. Key_Size);

   type Bound_Object;
   type Bound_Object_Ptr is access Bound_Object;

   type Bound_Object is record
      Key  : String_Ptr;
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
