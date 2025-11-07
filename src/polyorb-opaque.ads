------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . O P A Q U E                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Utility declarations for low-level memory management.

with Ada.Streams;
with PolyORB.Utils.Unchecked_Deallocation;
with System;

package PolyORB.Opaque is

   pragma Preelaborate;

   ----------------------------------------
   -- All-purpose memory allocation type --
   ----------------------------------------

   type Zone_Access is access all Ada.Streams.Stream_Element_Array;
   --  A storage zone: an array of bytes.

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Ada.Streams.Stream_Element_Array,
      Name => Zone_Access);

   --------------------------------------
   -- All-purpose memory location type --
   --------------------------------------

   subtype Opaque_Pointer is System.Address;

   function Is_Null (P : Opaque_Pointer) return Boolean;
   pragma Inline (Is_Null);

end PolyORB.Opaque;
