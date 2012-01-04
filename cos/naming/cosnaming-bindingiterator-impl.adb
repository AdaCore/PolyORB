------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S N A M I N G . B I N D I N G I T E R A T O R . I M P L        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

with CosNaming.BindingIterator.Skel;
pragma Warnings (Off, CosNaming.BindingIterator.Skel);

package body CosNaming.BindingIterator.Impl is

   Null_Binding : constant Binding := (To_Sequence (0), nobject);

   procedure Free is new Ada.Unchecked_Deallocation
     (Bindings.Element_Array, Binding_Element_Array_Ptr);

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr is
      Obj : Object_Ptr;

   begin
      Obj := new Object;
      Obj.Self := Obj;
      PTM.Create (Obj.Mutex);

      return Obj;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : access Object) is
   begin
      PTM.Enter (Self.Mutex);
      if Self.Table /= null then
         Free (Self.Table);
      end if;
      PTM.Leave (Self.Mutex);
   end Destroy;

   --------------
   -- Next_One --
   --------------

   procedure Next_One
     (Self    : access Object;
      B       : out CosNaming.Binding;
      Returns : out CORBA.Boolean) is
   begin
      PTM.Enter (Self.Mutex);

      if Self.Index <= Self.Table'Last then
         B := Self.Table (Self.Index);
         Self.Index := Self.Index + 1;
         Returns := True;

      else
         B := Null_Binding;
         Returns := False;
      end if;

      PTM.Leave (Self.Mutex);
   end Next_One;

   ------------
   -- Next_N --
   ------------

   procedure Next_N
     (Self     : access Object;
      How_Many : CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      Returns  : out CORBA.Boolean)
   is
      First : Natural renames Self.Index;
      Last  : Natural;

   begin
      PTM.Enter (Self.Mutex);

      Last := Self.Index + Natural (How_Many) - 1;
      if Last <= Self.Table'Last then
         BL := BindingList (Bindings.To_Sequence (Self.Table (First .. Last)));
         Self.Index := Last + 1;
         Returns := True;

      else
         Returns := False;
      end if;

      PTM.Leave (Self.Mutex);
   end Next_N;

end CosNaming.BindingIterator.Impl;
