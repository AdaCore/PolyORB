------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . G A R L I C . F I L T E R S . N O N E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;

pragma Warnings (Off);
with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);
pragma Warnings (On);

with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Filters.None is

   None_Filter : aliased None_Filter_Type;
   --  Dummy instance of None_Filter, only used for dispatching

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
     (Filter : in None_Filter_Type;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is
   begin
      return new Stream_Element_Array'(Stream);
   end Filter_Incoming;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     None_Filter_Type;
      Params : in     Filter_Params_Access;
      Stream : access System.RPC.Params_Stream_Type)
      return Stream_Element_Access is
   begin
      return To_Stream_Element_Access (Stream);
   end Filter_Outgoing;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
     (Filter : None_Filter_Type;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is
   begin
      return null;
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
     (Filter : None_Filter_Type;
      Params : Filter_Params_Access) return Stream_Element_Access is
   begin
      return null;
   end Filter_Params_Write;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (Filter          : in  None_Filter_Type;
      Public_Params   : out Filter_Params_Access;
      Private_Params  : out Filter_Params_Access;
      Exchange_Params : out Boolean) is
   begin
      Public_Params   := null;
      Private_Params  := null;
      Exchange_Params := False;
   end Generate_Params;

begin
   Register_Filter (null, "none");
end System.Garlic.Filters.None;
