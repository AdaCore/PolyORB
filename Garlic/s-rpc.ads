------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                           S Y S T E M . R P C                            --
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
with System.Garlic.Streams;

package System.RPC is

   type Partition_ID is range 0 .. 63;
   --  This type must not be modified without checking the code in
   --  a-except.adb, since it expects a Partition_ID whose string
   --  representation fits on two characters.

   Communication_Error : exception;

   type Params_Stream_Type
     (Initial_Size : Ada.Streams.Stream_Element_Count) is new
       Ada.Streams.Root_Stream_Type with private;

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   procedure Do_RPC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);
   --  Synchronous call

   procedure Do_APC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type);
   --  Asynchronous call

   type RPC_Receiver is
     access procedure
       (Params     : access Params_Stream_Type;
        Result     : access Params_Stream_Type);
   --  Handled used for incoming RPC

   procedure Establish_RPC_Receiver (
      Partition : in Partition_ID;
      Receiver  : in RPC_Receiver);

private

   pragma Inline (Read);
   pragma Inline (Write);

   type Params_Stream_Type (Initial_Size : Ada.Streams.Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type with record
        X : aliased System.Garlic.Streams.Params_Stream_Type (Initial_Size);
     end record;

   type Params_Stream_Access is access Params_Stream_Type;

   type Request_Id is mod 2 ** 8;
   --  The Request_Id identifies the request being sent

   type RPC_Opcode is (RPC_Request,
                       RPC_Answer,
                       RPC_Request_Cancellation,
                       RPC_Cancellation_Accepted,
                       APC_Request);
   --  Type of operation

   type Request_Header (Kind : RPC_Opcode) is record
      case Kind is
         when RPC_Request | RPC_Answer |
           RPC_Request_Cancellation | RPC_Cancellation_Accepted =>
            Id : Request_Id;
         when APC_Request =>
            null;
      end case;
   end record;

   procedure Insert_Request
     (Params : access System.Garlic.Streams.Params_Stream_Type;
      Header : in Request_Header);
   --  Add a Request_Header in front of Params

   procedure Initialize;
   --  Initialize the runtime of System.RPC

   procedure Shutdown;
   --  Shutdown System.RPC and its private child packages

   procedure When_Established;
   --  Wait for partition to be established.

end System.RPC;

