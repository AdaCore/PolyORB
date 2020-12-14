------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.SERVANT               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2020, Free Software Foundation, Inc.          --
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

with PolyORB.Minimal_Servant;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Requests;

package PolyORB.Services.Naming.NamingContext.Servant is

   Key_Size : constant := 4;
   type Key_Type is new String (1 .. Key_Size);

   type Bound_Object;
   type Bound_Object_Ptr is access Bound_Object;

   type Object is tagged;
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

   overriding procedure Invoke
     (Self     : access Object;
      Request  : PolyORB.Requests.Request_Access);
   function If_Desc return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Middleware 'glue'

   function Create return Object_Ptr;

end PolyORB.Services.Naming.NamingContext.Servant;
