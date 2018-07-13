------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . C O N T E X T L I S T                     --
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

--  Implementation Note: this package implements the recommendation of
--  the OMG issue #3706, that add new primitives to CORBA::Object.
--  See CORBA.Object package specifications for more details.

with CORBA.AbstractBase;
with CORBA.Impl;
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package CORBA.ContextList is

   pragma Elaborate_Body;

   type Ref is new CORBA.AbstractBase.Ref with null record;
   Nil_Ref : constant Ref;

   type Object is new CORBA.Impl.Object with private;
   type Object_Ptr is access all Object;

   overriding procedure Finalize (Obj : in out Object);

   function Get_Count (Self : Ref) return CORBA.Unsigned_Long;

   procedure Add (Self : Ref; Exc : CORBA.String);

   function Item
     (Self  : Ref;
      Index : CORBA.Unsigned_Long)
     return CORBA.String;

   procedure Remove (Self : Ref; Index : CORBA.Unsigned_Long);

   function Create_Object return Object_Ptr;

private

   --  The actual implementation of an ContextList: an unbounded
   --  sequence of CORBA.String

   package Context_Sequence is new CORBA.Sequences.Unbounded (CORBA.String);

   type Object is new CORBA.Impl.Object with record
     List : Context_Sequence.Sequence := Context_Sequence.Null_Sequence;
   end record;

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Ref with null record);

end CORBA.ContextList;
