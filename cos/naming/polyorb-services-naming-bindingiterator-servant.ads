------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.SERVICES.NAMING.BINDINGITERATOR.SERVANT              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Minimal_Servant;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Requests;

package PolyORB.Services.Naming.BindingIterator.Servant is

   pragma Elaborate_Body;

   package Bindings renames SEQUENCE_Binding;

   type Binding_Element_Array_Ptr is access Bindings.Element_Array;

   type Object;
   type Object_Ptr is access all Object;
   type Object is
     new PolyORB.Minimal_Servant.Servant with
      record
         Self  : Object_Ptr;
         Index : Natural;
         Table : Binding_Element_Array_Ptr;
      end record;

   function Create return Object_Ptr;

   procedure Invoke
     (Self     : access Object;
      Request  : in     PolyORB.Requests.Request_Access);
   --  Middleware 'glue'.

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Middleware 'glue'.

end PolyORB.Services.Naming.BindingIterator.Servant;
