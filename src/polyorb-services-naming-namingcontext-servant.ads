------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.SERVANT               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

with PolyORB.Minimal_Servant;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Requests;

package PolyORB.Services.Naming.NamingContext.Servant is

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
         Obj  : PolyORB.References.Ref;
         Prev : Bound_Object_Ptr;
         Next : Bound_Object_Ptr;
         NC   : Object_Ptr;
      end record;

   type Object is
     new PolyORB.Minimal_Servant.Servant with
      record
         Key  : Key_Type;
         Self : Object_Ptr;
         Prev : Object_Ptr;
         Next : Object_Ptr;
         Head : Bound_Object_Ptr;
         Tail : Bound_Object_Ptr;
      end record;

   procedure Invoke
     (Self     : access Object;
      Request  : PolyORB.Requests.Request_Access);
   function If_Desc return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Middleware 'glue'

   function Create return Object_Ptr;

end PolyORB.Services.Naming.NamingContext.Servant;
