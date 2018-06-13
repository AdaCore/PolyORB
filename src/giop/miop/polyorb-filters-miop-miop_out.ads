------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . F I L T E R S . M I O P . M I O P _ O U T         --
--                                                                          --
--                                 S p e c                                  --
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

--  MIOP filter for data which arrive from a GIOP Session

with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Filters.MIOP.MIOP_Out is

   pragma Elaborate_Body;

   type MIOP_Out_Factory is new Factory with private;

   overriding procedure Create
     (Fact     : access MIOP_Out_Factory;
      MIOP_Out :    out Filter_Access);

private

   type MIOP_Out_Factory is new Factory with null record;

   --  MIOP_OUT status
   type MIOP_Out_Filter is new Filter with record
      --  MIOP buffer
      MIOP_Buff : Buffers.Buffer_Access;
      --  Max size of MIOP packet
      Max_Size  : Types.Unsigned_Short;
   end record;

   overriding function Handle_Message
     (F : not null access MIOP_Out_Filter;
      S : Components.Message'Class) return Components.Message'Class;

end PolyORB.Filters.MIOP.MIOP_Out;
