------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Any;
with PolyORB.Initialization;
with PolyORB.Types;
with PolyORB.Utils.Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Test000 is

   use PolyORB.Any;
   use PolyORB.Utils.Report;
   use PolyORB.Types;

   procedure Simple_Test;

   -----------------
   -- Simple_Test --
   -----------------

   procedure Simple_Test
   is
      A : Any;

      Initial_Value : constant PolyORB.Types.Short
        := PolyORB.Types.Short (2);

      Value : PolyORB.Types.Short;
   begin
      A := To_Any (PolyORB.Types.Short (2));
      Value := From_Any (A);
      Output ("Any: Short", Value = Initial_Value);
   end Simple_Test;

begin
   PolyORB.Initialization.Initialize_World;
   Simple_Test;
   End_Report;
end Test000;
