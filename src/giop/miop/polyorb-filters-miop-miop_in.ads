------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . F I L T E R S . M I O P . M I O P _ I N        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2003 Free Software Foundation, Inc.            --
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

--  MIOP filter for data which arrive from network to ORB
--  This filter MUST be under a GIOP Session

with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Filters.MIOP.MIOP_In is

   pragma Elaborate_Body;

   type MIOP_In_Factory is new Factory with private;

   procedure Create
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

   function Handle_Message
     (F : access MIOP_In_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

   type MIOP_In_Factory is new Factory with null record;

end PolyORB.Filters.MIOP.MIOP_In;
