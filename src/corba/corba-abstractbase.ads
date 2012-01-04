------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   C O R B A . A B S T R A C T B A S E                    --
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

with PolyORB.References;

with CORBA.Impl;

package CORBA.AbstractBase is

   pragma Elaborate_Body;

   type Ref is new PolyORB.References.Ref with private;

--    procedure Set
--      (The_Ref : in out Ref;
--       The_Object : CORBA.Impl.Object_Ptr);
   --  Since CORBA.Impl.Object_Ptr is declared as a subtype
   --  of PolyORB.Smart_Pointers.Entity_Ptr, the Set operation
   --  is implicitly inherited from PolyORB.Smart_Pointers.Ref.

   function Object_Of (The_Ref : Ref) return CORBA.Impl.Object_Ptr;

   function Get (The_Ref : Ref) return CORBA.Impl.Object_Ptr
     renames Object_Of;

   --  The following primitive operations are inherited
   --  from PolyORB.Smart_Pointers.Ref.

   --    procedure Set
   --      (The_Ref : in out Ref;
   --       The_Entity : Ref_Ptr);

   --    procedure Unref (The_Ref : in out Ref)
   --      renames Finalize;

   --    function Is_Nil (The_Ref : Ref) return Boolean;
   --    function Is_Null (The_Ref : Ref) return Boolean
   --      renames Is_Nil;

   --    procedure Duplicate (The_Ref : in out Ref)
   --      renames Adjust;

   --    procedure Release (The_Ref : in out Ref);

   Nil_Ref : constant Ref;

private

   type Ref is new PolyORB.References.Ref with null record;
   Nil_Ref : constant Ref
     := (PolyORB.References.Ref with null record);

end CORBA.AbstractBase;
