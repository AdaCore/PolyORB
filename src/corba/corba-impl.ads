------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O R B A . I M P L                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Components;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;

package CORBA.Impl is

   pragma Elaborate_Body;

   type Object is abstract new PolyORB.Smart_Pointers.Entity
     with private;
   subtype Object_Ptr is PolyORB.Smart_Pointers.Entity_Ptr;
   --  Object_Ptr is the return type of CORBA.AbstractBase.Object_Of.
   --  It may either designate an actual local object
   --  (a CORBA.Impl.Object'Class), or a surrogate thereof
   --  (a D.SP.Entity'Class not derived from CORBA.Impl.Object).

   function Execute_Servant
     (Self : access Object;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   function To_PolyORB_Servant (S : access Object)
     return PolyORB.Servants.Servant_Access;

   function To_CORBA_Servant (S : PolyORB.Servants.Servant_Access)
     return Object_Ptr;

private

   type Implementation (As_Object : access Object'Class)
   is new PolyORB.Servants.Servant with null record;
   --  The CORBA personality is based on the Portable Object Adapter.

   function "=" (X, Y : Implementation) return Boolean;
   --  ??? XXX Why does the compiler require the presence of this operator?
   --  As a descendant of Component, Implementation is a limited type!

   function Execute_Servant
     (Self : access Implementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   type Object is abstract new PolyORB.Smart_Pointers.Entity with
   record
      Neutral_View : aliased Implementation (Object'Access);
      --  The PolyORB (personality-neutral) view of this servant.
      --  See also PolyORB.Minimal_Servant.
   end record;

end CORBA.Impl;
