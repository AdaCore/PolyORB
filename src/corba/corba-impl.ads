------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O R B A . I M P L                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

pragma Ada_2005;

with PolyORB.Requests;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Smart_Pointers.Controlled_Entities;

package CORBA.Impl is

   pragma Elaborate_Body;

   package PSPCE renames PolyORB.Smart_Pointers.Controlled_Entities;

   type Object is abstract new PSPCE.Entity with private;

   subtype Object_Ptr is PolyORB.Smart_Pointers.Entity_Ptr;
   --  Object_Ptr is the return type of CORBA.AbstractBase.Object_Of.
   --  It may either designate an actual local object (CORBA.Impl.Object'Class)
   --  or a surrogate thereof.

   function Execute_Servant
     (Self : not null access Object;
      Req  : PolyORB.Requests.Request_Access) return Boolean;

   function To_PolyORB_Servant
     (S : access Object)
     return PolyORB.Servants.Servant_Access;

   package Internals is

      --  Internal implementation subprograms. These shall not be
      --  used outside of PolyORB.

      function To_CORBA_Servant
        (S : PolyORB.Servants.Servant_Access)
        return Object_Ptr;

   end Internals;

private

   type Implementation (As_Object : access Object'Class) is
     new PolyORB.Servants.Servant with null record;
   --  The CORBA personality is based on the Portable Object Adapter.

   overriding function Execute_Servant
     (Self : not null access Implementation;
      Req  : PolyORB.Requests.Request_Access) return Boolean;

   type Object is abstract new PSPCE.Entity with record
      Neutral_View : aliased Implementation (Object'Access);
      --  The PolyORB (personality-neutral) view of this servant.
      --  See also PolyORB.Minimal_Servant.
   end record;

end CORBA.Impl;
