------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--         C O S N A M I N G . N A M I N G C O N T E X T . I M P L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;
with PortableServer;

package CosNaming.NamingContext.Impl is

   Key_Size : constant := 4;
   type Key_Type is new String (1 .. Key_Size);

   type Bound_Object;
   type Bound_Object_Ptr is access Bound_Object;

   type Object;
   type Object_Ptr is access all Object'Class;

   type Bound_Object is
      record
         BN   : NameComponent;
         BT   : BindingType;
         Obj  : CORBA.Object.Ref;
         Prev : Bound_Object_Ptr;
         Next : Bound_Object_Ptr;
         NC   : Object_Ptr;
      end record;

   type Object is
     new PortableServer.Servant_Base with
      record
         Key  : Key_Type;
         Self : Object_Ptr;
         Prev : Object_Ptr;
         Next : Object_Ptr;
         Head : Bound_Object_Ptr;
         Tail : Bound_Object_Ptr;
      end record;

   procedure Bind
     (Self : access Object;
      N    : in CosNaming.Name;
      Obj  : in CORBA.Object.Ref);

   procedure Rebind
     (Self : access Object;
      N    : in CosNaming.Name;
      Obj  : in CORBA.Object.Ref);

   procedure Bind_Context
     (Self : access Object;
      N    : in CosNaming.Name;
      NC   : in CosNaming.NamingContext.Ref);

   procedure Rebind_Context
     (Self : access Object;
      N    : in CosNaming.Name;
      NC   : in CosNaming.NamingContext.Ref);

   function Resolve
     (Self : access Object;
      N    : in CosNaming.Name)
     return CORBA.Object.Ref;

   procedure Unbind
     (Self : access Object;
      N    : in CosNaming.Name);

   function New_Context
     (Self : access Object)
     return CosNaming.NamingContext.Ref;

   function Create
     return CosNaming.NamingContext.Impl.Object_Ptr;

   function Bind_New_Context
     (Self : access Object;
      N    : in CosNaming.Name)
     return CosNaming.NamingContext.Ref;

   procedure Destroy
     (Self : access Object);

   procedure List
     (Self     : access Object;
      How_Many : in CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      BI       : out CosNaming.BindingIterator_Forward.Ref);

end CosNaming.NamingContext.Impl;
