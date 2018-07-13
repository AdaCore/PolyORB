------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O R B A . I M P L                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

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

   overriding procedure Finalize (O : in out Object);

end CORBA.Impl;
