------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A L L _ F U N C T I O N S . I M P L                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with all_functions.Skel;
pragma Warnings (Off, all_functions.Skel);

with CORBA.ORB;

package body all_functions.Impl is
   use CORBA; --  WAG:5.03

   Oneway_Value : CORBA.Short := 0;

   function Get_the_attribute
     (Self : access Object)
      return CORBA.Short
   is
   begin
      return Self.Attribute;
   end Get_the_attribute;

   procedure Set_the_attribute
     (Self : access Object;
      To   : CORBA.Short)
   is
   begin
      Self.Attribute := To;
   end Set_the_attribute;

   function Get_the_readonly_attribute
     (Self : access Object)
      return CORBA.Short
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return 18;
   end Get_the_readonly_attribute;

   procedure void_proc
     (Self : access Object)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      null;
   end void_proc;

   procedure in_proc
     (Self : access Object;
      a : CORBA.Short;
      b : CORBA.Short;
      c : CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, a, b, c);
      pragma Warnings (On);
   begin
      null;
   end in_proc;

   procedure out_in_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : CORBA.Long)
   is
      pragma Unreferenced (Self);
   begin
      a := CORBA.Short (b) + 1;
   end out_in_proc;

   procedure out_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : out CORBA.Short;
      c : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      a := 10;
      b := 11;
      c := 12;
   end out_proc;

   procedure inout_proc
     (Self : access Object;
      a : in out CORBA.Short;
      b : in out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      a := a + 1;
      b := b + 1;
   end inout_proc;

   procedure in_out_proc
     (Self : access Object;
      a : CORBA.Short;
      b : CORBA.Short;
      c : out CORBA.Short;
      d : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, a, b);
      pragma Warnings (On);
   begin
      c := 3;
      d := 4;
   end in_out_proc;

   procedure in_inout_proc
     (Self : access Object;
      a : CORBA.Short;
      b : in out CORBA.Short;
      c : CORBA.Short;
      d : in out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, a, c);
      pragma Warnings (On);
   begin
      b := 36;
      d := 40;
   end in_inout_proc;

   procedure out_inout_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : in out CORBA.Short;
      c : in out CORBA.Short;
      d : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Entry_B : constant CORBA.Short := b;
      Entry_C : constant CORBA.Short := c;
   begin
      a := 111;
      b := Entry_C + 1;
      c := Entry_B + 1;
      d := 999;
   end out_inout_proc;

   procedure in_out_inout_proc
     (Self : access Object;
      a : CORBA.Short;
      b : out CORBA.Short;
      c : in out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, a);
      pragma Warnings (On);
   begin
      b := -54;
      c := c + 1;
   end in_out_inout_proc;

   function void_fun
     (Self : access Object)
      return CORBA.Short
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return 3;
   end void_fun;

   function in_fun
     (Self : access Object;
      a : CORBA.Short;
      b : CORBA.Short;
      c : CORBA.Short)
      return CORBA.Short
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, a, b, c);
      pragma Warnings (On);
   begin
      return 7;
   end in_fun;

   procedure out_fun
     (Self : access Object;
      a : out CORBA.Short;
      b : out CORBA.Short;
      c : out CORBA.Short;
      Returns : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      a := 5;
      b := 6;
      c := 7;
      Returns := 10;
   end out_fun;

   procedure inout_fun
     (Self : access Object;
      a : in out CORBA.Short;
      b : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      a := a + 1;
      b := b + 1;
      Returns := a + b;
   end inout_fun;

   procedure in_out_fun
     (Self : access Object;
      a : CORBA.Short;
      b : CORBA.Short;
      c : out CORBA.Short;
      d : out CORBA.Short;
      Returns : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      c := b;
      d := a;
      Returns := a + b;
   end in_out_fun;

   procedure in_inout_fun
     (Self : access Object;
      a : CORBA.Short;
      b : in out CORBA.Short;
      c : CORBA.Short;
      d : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      b := b + a;
      d := d + c;
      Returns := b + d;
   end in_inout_fun;

   procedure out_inout_fun
     (Self : access Object;
      a : out CORBA.Short;
      b : in out CORBA.Short;
      c : in out CORBA.Short;
      d : out CORBA.Short;
      Returns : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      a := b;
      b := b + 1;
      d := c;
      c := c + 1;
      Returns := a + b + c + d + 1;
   end out_inout_fun;

   procedure in_out_inout_fun
     (Self : access Object;
      a : CORBA.Short;
      b : out CORBA.Short;
      c : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      b := a + 1;
      c := a + c;
      Returns := -1;
   end in_out_inout_fun;

   procedure oneway_void_proc
     (Self : access Object)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      Oneway_Value := 1;
      delay 1.0;
      Oneway_Value := 2;
   end oneway_void_proc;

   procedure oneway_in_proc
     (Self : access Object;
      a : CORBA.Short;
      b : CORBA.Short)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      Oneway_Value := a;
      delay 1.0;
      Oneway_Value := b;
   end oneway_in_proc;

   function oneway_checker (Self : access Object) return CORBA.Short is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return Oneway_Value;
   end oneway_checker;

   procedure StopServer (Self : access Object) is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      CORBA.ORB.Shutdown (Wait_For_Completion => False);
   end StopServer;

end all_functions.Impl;
