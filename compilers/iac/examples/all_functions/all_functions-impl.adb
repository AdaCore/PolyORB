------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A L L _ F U N C T I O N S . I M P L                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with all_functions.Skel;
pragma Warnings (Off, all_functions.Skel);

package body all_functions.Impl is

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
   begin
      a := 45;
      b := 46;
      c := 47;
      d := 48;
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
      delay 5.0;
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
      delay 5.0;
      Oneway_Value := b;
   end oneway_in_proc;

   function oneway_checker (Self : access Object) return CORBA.Short is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return Oneway_Value;
   end oneway_checker;

end all_functions.Impl;
