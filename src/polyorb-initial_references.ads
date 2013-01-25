------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . I N I T I A L _ R E F E R E N C E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2013, Free Software Foundation, Inc.          --
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

--  Support package for initial references

with PolyORB.References;
with PolyORB.Utils.Strings.Lists;

package PolyORB.Initial_References is

   type Create_Ptr is access function return PolyORB.References.Ref;
   --  Allocator type

   procedure Register_Initial_Reference
     (Id        : Standard.String;
      Allocator : Create_Ptr);
   --  Register (Id, Allocator) tuple

   procedure Register_Initial_Reference
     (Id  : Standard.String;
      Ref : PolyORB.References.Ref);
   --  Register (Id, Ref) tuple

   function Resolve_Initial_References
     (Id : Standard.String) return PolyORB.References.Ref;
   --  Return a valid reference to an object if Id has been previously
   --  registred.
   --  If Id has been registred with a Ref, then returns it.
   --  If Id has been registred with an allocator, use this allocator
   --  to create a reference.

   function List_Initial_Services
     return PolyORB.Utils.Strings.Lists.List;
   --  List all registered references.

end PolyORB.Initial_References;
