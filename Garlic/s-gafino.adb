------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . F I L T E R S . N O N E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

-- This  package  is part of  the  transparent data filtering  extension to --
-- GARLIC  developed at  the  Software Engineering Laboratory of the  Swiss --
-- Federal Institute of Technology in Lausanne (EPFL).                      --

with Ada.Streams; use Ada.Streams;

with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);

with System.Garlic.Utils;

use System.Garlic.Utils;

package body System.Garlic.Filters.None is

   Dummy : aliased No_Filter;
   --  Dummy instance of No_Filter, only used for dispatching

   Dummy_Params : aliased No_Filter_Params;
   --  Dummy filter parameters, allowing the use of this filter even as
   --  the default filter.

   Dummy_Name : constant String := "none";

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
     (Filter : in     No_Filter;
      Params : in     Filter_Params_Access;
      Stream : access System.RPC.Params_Stream_Type)
      return Stream_Element_Array is
   begin
      return To_Stream_Element_Array (Stream);
   end Filter_Outgoing;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
     (Filter : in No_Filter;
      Params : in Filter_Params_Access;
      Stream : in Ada.Streams.Stream_Element_Array)
      return Ada.Streams.Stream_Element_Array is
   begin
      return Stream;
   end Filter_Incoming;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (Filter : in No_Filter;
      Params : out Filter_Params_Access;
      Private_Params : out Filter_Params_Access;
      Needs_Key_Exchange : out Boolean) is
   begin
      Params := Dummy_Params'Access;
      Private_Params := Dummy_Params'Access;
      Needs_Key_Exchange := false;
   end Generate_Params;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
     (Filter : No_Filter;
      Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is
      S : aliased System.RPC.Params_Stream_Type (Stream'Length);
      P : No_Filter_Params;
   begin
      To_Params_Stream_Type (Stream, S'Access);
      No_Filter_Params'Read (S'Access, P);
      return new No_Filter_Params'(P);
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
     (Filter : No_Filter;
      P : Filter_Params_Access) return
      Ada.Streams.Stream_Element_Array is
      S : aliased System.RPC.Params_Stream_Type (32);
   begin
      No_Filter_Params'Write (S'Access, No_Filter_Params (P.all));
      return To_Stream_Element_Array (S'Access);
   end Filter_Params_Write;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Filter : No_Filter)
      return String is
   begin
      return Dummy_Name;
   end Get_Name;

   ------------------
   -- Print_Params --
   ------------------

   procedure Print_Params (Params : No_Filter_Params) is
   begin
      null;
   end Print_Params;


begin
   Register_Filter (Dummy'Access);
end System.Garlic.Filters.None;
