------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
-- S Y S T E M . G A R L I C . S E R I A L . M A C H I N E _ S P E C I F I C  --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

private package System.Garlic.Serial.Machine_Specific is

   --  This package contains definition for machine specific access
   --  to a serial device. The body of this package must provide a way
   --  of sending and receiving reliably data over a serial link.

   type Serial_Device is private;

   function Open (Description : String) return Serial_Device;
   --  Open a device and return an handler on it. If it's impossible,
   --  then System.RPC.Communication_Error must be raised.

   procedure Close (Device : in Serial_Device);
   --  Close a serial device.

   procedure Send
     (Device : in Serial_Device;
      Data   : access Ada.Streams.Stream_Element_Array);
   --  Send data over a serial link. System.RPC.Communication_Error must be
   --  raised if it is impossible to send data.

   procedure Receive
     (Device : in Serial_Device;
      Data   : access Ada.Streams.Stream_Element_Array);
   --  Receive data from a serial link. Exactly Data'Length bytes will
   --  be received. If it is impossible to receive data, then
   --  System.RPC.Communication_Error must be raised.

private

   type Serial_Device is new Natural;

end System.Garlic.Serial.Machine_Specific;
