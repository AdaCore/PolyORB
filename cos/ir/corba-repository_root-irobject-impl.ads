------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  C O R B A . R E P O S I T O R Y _ R O O T . I R O B J E C T . I M P L   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with PortableServer;

package CORBA.Repository_Root.IRObject.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind);

   function Get_Real_Object (Self : access Object)
     return Object_Ptr;

   function get_def_kind
     (Self : access Object)
     return CORBA.Repository_Root.DefinitionKind;

   procedure destroy
     (Self : access Object);

private

   --  The "real_object" is a pointer to the real object
   --  The Def_Kind is an attribute
   type Object is
     new PortableServer.Servant_Base with record
        Real_Object : Object_Ptr;
        Def_Kind : CORBA.Repository_Root.DefinitionKind;
     end record;

   -------------------
   -- IRObject list --
   -------------------

   type IRObject_List_Cell;
   type IRObject_List is access IRObject_List_Cell;
   type IRObject_List_Cell is record
      Car : Object_Ptr;
      Cdr : IRObject_List;
   end record;

   Nil_List : constant IRObject_List := null;

   type IRObject_Iterator is new IRObject_List;

end CORBA.Repository_Root.IRObject.Impl;
