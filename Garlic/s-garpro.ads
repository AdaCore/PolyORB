------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . P R O T O C O L S                --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with System.RPC;

package System.Garlic.Protocols is

   type Protocol_Type is abstract tagged limited null record;
   --  New protocols must derivate from this tagged type. They must
   --  implement versions of the procedures below which are marked as
   --  abstract

   type Protocol_Access is access all Protocol_Type'Class;
   --  Pointer on Protocol_Type or derivative

   --  Each protocol must define a 'Create' function which returns a
   --  Protocol_Access instance which is in fact a protocol instance.
   --  This will be called from System.Garlic.Startup.

   function Get_Name (P : access Protocol_Type) return String
     is abstract;
   --  A string returning the name of the protocol.

   procedure Set_Boot_Data
     (Protocol         : access Protocol_Type;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False)
     is abstract;
   --  Set the boot data. This will be called as Set_Boot_Data (Protocol)
   --  if we are not the boot protocol. You should be prepared to deal with
   --  incomplete data if you are the master.

   function Get_Info (P : access Protocol_Type) return String;
   --  Return a string which holds enough information to be usable by
   --  another partition to contact us. Default is an empty string.

   Unused_Space : constant Ada.Streams.Stream_Element_Count := 32;
   --  The number of unused slots that are stored whenever Send is called.
   --  This is used to add the extra arguments that may be needed by a
   --  protocol for its private use.

   procedure Send
     (Protocol  : access Protocol_Type;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array) is abstract;
   --  Send data to a remote partition. See comment about Unused_Space
   --  above.

   procedure Shutdown (Protocol : access Protocol_Type) is abstract;
   --  When this procedure gets called, the protocol must terminate
   --  as soon as possible and terminate cleanly.

end System.Garlic.Protocols;

