------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . L O O P B A C K                --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams;
with System.Garlic.Protocols;
with System.RPC;

package System.Garlic.Loopback is

   type Loopback_Protocol is
     new System.Garlic.Protocols.Protocol_Type with private;
   --  Loopback protocol

   function Create return System.Garlic.Protocols.Protocol_Access;

   function Get_Name (P : access Loopback_Protocol) return String;
   --  Get name

   procedure Set_Boot_Data
     (Protocol         : access Loopback_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False);
   --  Set boot data

   function Get_Info (P : access Loopback_Protocol) return String;
   --  Get protocol specific info

   procedure Send
     (Protocol  : access Loopback_Protocol;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array);
   --  Send data

   procedure Shutdown (Protocol : access Loopback_Protocol);
   --  Shutdown protocol

private

   type Loopback_Protocol is
      new System.Garlic.Protocols.Protocol_Type with null record;

end System.Garlic.Loopback;
