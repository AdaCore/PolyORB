------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . F I L T E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
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

with Ada.Streams;
with System.RPC;
with System.Garlic.Heart;
with System.Garlic.Streams;

package System.Garlic.Filters is

   pragma Elaborate_Body;

   function Filter_Outgoing
      (To_Partition : in     System.RPC.Partition_ID;
       Operation    : in     System.Garlic.Heart.Opcode;
       Params       : access System.RPC.Params_Stream_Type)
      return Streams.Stream_Element_Access;

   function Filter_Incoming
      (From_Partition : in System.RPC.Partition_ID;
       Operation      : in System.Garlic.Heart.Opcode;
       Params         : in Ada.Streams.Stream_Element_Array)
      return Streams.Stream_Element_Access;
   --  The two functions above are called by Garlic.Heart for all
   --  communications.
   --     NOTE: I would have liked very much to have a symmetric interface
   --  with 'Outgoing' taking a stream and returning an array while 'Incoming'
   --  would take an array and return a stream. Unfortunately, this is not
   --  possible because Ada.Streams.Root_Stream_Type is tagged limited.

   procedure Initialize;
   --  Elaboration code.

   procedure Set_Channel_Filter (Partition, Filter : in String);
   --  Tells this package which filter to use for communicating with a
   --  certain partition.

   procedure Set_Default_Filter (Filter : in String);
   --  Sets the default filter. This filter will be used on links for
   --  which no filter is defined using 'Set_Channel_Filter'. If this
   --  procedure is not called, no filtering will be done by default.

   procedure Set_Registration_Filter (Filter : in String);
   --  Sets the registration filter. This filter is used to send channel-
   --  specific filter-internal data to the partition on the other end of
   --  the channel the first time a filter is used on the channel.

   Filter_Already_Registered : exception;
   --  Raised when there are several filters trying to register with the
   --  same name.

   Too_Many_Filters,
   Not_A_Public_Params_Algorithm : exception;
   --  Raised if we try to use a filter that needs to exchange its
   --  parameters, but whose parameters haven't yet arrived.

private

   type Filter_Type   is abstract tagged limited null record;
   --  New filter algorithm packages must derivate from this tagged
   --  type. They must implement versions of all following procedures that
   --  are marked as abstract. (Since this type is private, implement new
   --  filters in child packages of this package.)

   type Filter_Access is access all Filter_Type'Class;
   --  Access to Filter_Type or derivative

   type Filter_Params is abstract tagged null record;
   --  With each filter, some parameters may be associated on a per-channel
   --  basis by derivating from this type. It is therefore possible to use
   --  the same filter with diffferent parameters on different channels. If
   --  necessary, this package will transmit those parameters to the
   --  partition at the other end of the channel (see 'Generate_Params'
   --  below).

   type Filter_Params_Access is access all Filter_Params'Class;
   --  Access to Filter_Params or derivative

   No_Filter_Name : aliased String := "none";
   --  Initially, all channels are initialized to use the filter with
   --  this name. There *must* be a filter with that name, and all
   --  calls to 'Set_Channel_Filter' must be made on both ends before
   --  the first message is ever sent through that channel. (Note:
   --  this implementation doesn't yet incorporate protocols for
   --  negociating a change of the filter algorithm or even the filter
   --  parameters at run-time.)

   function Filter_Outgoing
      (Filter : in     Filter_Type;
       Params : in     Filter_Params_Access;
       Stream : access System.RPC.Params_Stream_Type)
      return Streams.Stream_Element_Access
      is abstract;
   --  Run the unfiltered data in 'Stream' through the filter and return
   --  the result.

   function Filter_Incoming
      (Filter : in Filter_Type;
       Params : in Filter_Params_Access;
       Stream : in Ada.Streams.Stream_Element_Array)
      return Streams.Stream_Element_Access
      is abstract;
   --  Run the filtered data in 'Stream' through the inverse filter and
   --  return the resulting unfiltered data.

   function Filter_Params_Read
      (Filter : Filter_Type;
       Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access
      is abstract;

   function Filter_Params_Write
      (Filter : Filter_Type;
       Params : Filter_Params_Access)
      return Streams.Stream_Element_Access
      is abstract;

   procedure Generate_Params
      (Filter                : in  Filter_Type;
       Params                : out Filter_Params_Access;
       Private_Params        : out Filter_Params_Access;
       Needs_Params_Exchange : out Boolean)
      is abstract;
   --  This procedure must generate new filter parameters. It may always
   --  deliver the same parameters (hard-coded constant) or generate new
   --  ones each time it is called.
   --     If the algorithm is a public parameter algorithm, the parameters
   --  returned in 'Params' should be the public ones, and those returned
   --  in 'Private_Params' the private ones. If your filter algorithm uses
   --  only one set of parameters, it may return null in 'Private_Params'.
   --  The Boolean 'Needs_Params_Exchange' should be set to true if you
   --  want to use the provided parameter exchange facility to communicate
   --  the new 'Params' to your destination partition.

   function Get_Name
      (Filter : Filter_Type)
      return String is abstract;
   --  Has to return the filter package's name (e.g. "LZH")

   procedure Print_Params (Params : Filter_Params) is abstract;
   --  For debugging only.

   procedure Register_Filter
      (Filter : in Filter_Access);
   --  Has to be called by a new filter package to register itself in the
   --  filter database. Note: this is not a primitiove operation of type
   --  'Filter_Type'.
   --
   --  Exceptions:
   --    Filter_Already_Registered =>
   --                   a filter package tries to register more than once.
   --    Too_Many_Filters =>
   --                   there's no more space left in the internal table of
   --                   installed filters.

end System.Garlic.Filters;









