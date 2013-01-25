------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . F I L T E R S . M I O P . M I O P _ I N          --
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

pragma Ada_2005;

--  MIOP filter for data which arrive from network to ORB
--  This filter MUST be under a GIOP Session

with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Filters.MIOP.MIOP_In is

   pragma Elaborate_Body;

   type MIOP_In_Factory is new Factory with private;

   overriding procedure Create
     (Fact     : access MIOP_In_Factory;
      MIOP_In :    out Filter_Access);

private

   --  MIOP_In state
   type MIOP_State is
     (Wait_For_GIOP_Layer,   --  Wait for upper GIOP layer
      Wait_MIOP_Header,      --  Wait a new MIOP header
      Wait_Unique_Id,        --  Wait for Unique Id
      Wait_GIOP_Ask,         --  Wait for upper GIOP layer
      Wait_GIOP_Data         --  Wait for data to GIOP layer from lower
      );

   --  MIOP stack status
   type MIOP_In_Filter is new Filter with record
      --  MIOP status
      State            : MIOP_State := Wait_For_GIOP_Layer;
      --  MIOP buffer
      MIOP_Buf         : Buffers.Buffer_Access;
      --  GIOP buffer
      In_Buf           : Buffers.Buffer_Access;
      --  Data expected by GIOP layer
      Data_Exp         : Stream_Element_Count;
      --  Data expected by GIOP layer at demand
      Initial_Data_Exp : Stream_Element_Count;
      --  Data remaining in GIOP buffer at demand
      Initial_Remain   : Stream_Element_Count;
      --  Header of current MIOP packet
      Header           : MIOP_Header;
      --  Previous MIOP packet Header
      Old_Header       : MIOP_Header;
      --  Indicate if we are in fragment mode
      Fragment         : Boolean := False;
      --  Payload of current MIOP packet
      Payload          : Types.Unsigned_Short;
   end record;

   overriding function Handle_Message
     (F : not null access MIOP_In_Filter;
      S : Components.Message'Class) return Components.Message'Class;

   type MIOP_In_Factory is new Factory with null record;

end PolyORB.Filters.MIOP.MIOP_In;
