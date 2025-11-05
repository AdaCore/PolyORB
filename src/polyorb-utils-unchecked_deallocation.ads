------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . U T I L S . U N C H E C K E D _ D E A L L O C A T I O N --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2025, Free Software Foundation, Inc.               --
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

--  Generic unchecked deallocation utility
--
--  This package provides a generic procedure for deallocating dynamically
--  allocated objects. It is a thin wrapper around Ada.Unchecked_Deallocation
--  that consolidates the 74+ duplicate Free procedure instantiations
--  throughout the PolyORB codebase into a single, reusable utility.
--
--  Usage:
--    with PolyORB.Utils.Unchecked_Deallocation;
--
--    type My_Type is ...;
--    type My_Type_Access is access all My_Type;
--
--    procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
--      (Object => My_Type, Name => My_Type_Access);
--
--  This is functionally equivalent to:
--    procedure Free is new Ada.Unchecked_Deallocation
--      (My_Type, My_Type_Access);
--
--  Benefits:
--  - Reduces code duplication (74 instances → 1 generic)
--  - Centralizes memory management pattern
--  - Improves maintainability
--  - Makes future refactoring easier (e.g., adding debug hooks)

with Ada.Unchecked_Deallocation;

package PolyORB.Utils.Unchecked_Deallocation is

   pragma Preelaborate;

   generic
      type Object (<>) is limited private;
      type Name is access Object;
   procedure Free (X : in out Name);
   pragma Inline (Free);
   --  Generic deallocation procedure. This is a thin wrapper around
   --  Ada.Unchecked_Deallocation that provides zero runtime overhead
   --  (inlined). The generic parameters match the standard
   --  Ada.Unchecked_Deallocation signature.
   --
   --  After instantiation, calling Free(X) will:
   --  1. Deallocate the object designated by X
   --  2. Set X to null
   --
   --  Note: This procedure is unchecked and unsafe. Use with caution.
   --  Dangling pointers and double-free errors are possible if misused.

end PolyORB.Utils.Unchecked_Deallocation;
