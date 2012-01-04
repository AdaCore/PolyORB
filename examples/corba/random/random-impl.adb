------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          R A N D O M . I M P L                           --
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

with Ada.Numerics.Discrete_Random;

with Random.Skel;
pragma Warnings (Off, Random.Skel);
--  No entity from Random.Skel is referenced.

package body Random.Impl is

   type l48 is range 0 .. 2 ** 31 - 1;
   package l48_Random is new Ada.Numerics.Discrete_Random (l48);
   l48_Gen : l48_Random.Generator;

   function lrand48 (Self : access Object)
     return CORBA.Long is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return CORBA.Long (l48_Random.Random (l48_Gen));
   end lrand48;

   type m48 is range -2 ** 31 .. 2 ** 31 - 1;
   package m48_Random is new Ada.Numerics.Discrete_Random (m48);
   m48_Gen : m48_Random.Generator;

   function mrand48 (Self : access Object)
     return CORBA.Long is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return CORBA.Long (m48_Random.Random (m48_Gen));
   end mrand48;

begin
   l48_Random.Reset (l48_Gen);
   m48_Random.Reset (m48_Gen);
end Random.Impl;
