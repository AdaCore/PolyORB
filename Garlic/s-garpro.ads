------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . P R O T O C O L S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with GNAT.Strings;

with System.Garlic.Exceptions;
with System.Garlic.Types;

package System.Garlic.Protocols is

   type Protocol_Type is abstract tagged limited null record;
   --  New protocols must derivate from this tagged type. They must
   --  implement versions of the procedures below which are marked as
   --  abstract.
   --  Each protocol must define a 'Create' function which returns a
   --  Protocol_Access instance which is in fact a protocol instance.
   --  This will be called from System.Garlic.Startup.

   type Protocol_Access is access all Protocol_Type'Class;
   --  Pointer on Protocol_Type and any child

   Null_Protocol  : constant := 0;
   First_Protocol : constant := 1;
   Last_Protocol  : Natural := Null_Protocol;
   Max_Protocols  : constant := 10;
   Protocol_Table : array (First_Protocol .. Max_Protocols) of Protocol_Access;

   procedure Activate
     (Protocol : access Protocol_Type;
      Error    : in out Exceptions.Error_Type) is abstract;
   --  We first initialize the protocol to provide some data to accept
   --  incoming connections. The incomplete data provided have to be
   --  completed on initialization. This procedure is supposed to
   --  activate all the incoming connections and to accept incoming
   --  requests. For instance, this must be done prior to any
   --  communication with boot server.

   function Get_Name (Protocol : access Protocol_Type) return String
     is abstract;
   --  Return a string containing the name of the protocol

   procedure Initialize
     (Protocol  : access Protocol_Type;
      Self_Data : in String;
      Required  : in Boolean;
      Performed : out Boolean;
      Error     : in out Exceptions.Error_Type)
     is abstract;
   --  Initialize protocol. When Self_Data is non-null, use this
   --  location to receive messages. Required means that this
   --  initialization must be done. When Required is false, this
   --  initialization has to be performed only when the protocol was
   --  not previously initialized. This occurs when the current
   --  partition has no self location depending on this protocol. We
   --  have to initialize this protocol anyway, because it may be
   --  needed to contact other partitions. Incomplete data have to be
   --  completed once initialized.

   Forever : constant Duration := Duration'Last;
   Polling : constant Duration := 0.2;

   function Receive
     (Protocol  : access Protocol_Type;
      Timeout   : Duration)
     return Boolean is abstract;
   --  Try to receive any incoming stream, analyze and process
   --  it. Return False if Timeout expired.

   procedure Set_Boot_Data
     (Protocol  : access Protocol_Type;
      Boot_Data : in String;
      Error     : in out Exceptions.Error_Type)
     is abstract;
   --  When Boot_Data is non-null, use this location to contact boot
   --  partition.

   function Get_Data
     (Protocol : access Protocol_Type)
     return GNAT.Strings.String_List_Access;
   --  Return a string array which holds all the physical locations to
   --  be used by another partition to contact us.

   procedure Receive_From_All_Protocols;
   --  Receive from all protocols.

   procedure Register (Protocol : in Protocol_Access);
   --  Register the protocol as a present protocol

   Unused_Space : constant Ada.Streams.Stream_Element_Count := 32;
   --  The number of unused slots that are stored whenever Send is called.
   --  This is used to add the extra arguments that may be needed by a
   --  protocol for its private use.

   procedure Send
     (Protocol  : access Protocol_Type;
      Partition : in Types.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array;
      Error     : in out Exceptions.Error_Type) is abstract;
   --  Send data to a remote partition. See comment about Unused_Space
   --  above.

   procedure Shutdown (Protocol : access Protocol_Type) is abstract;
   --  The protocol must ensure that all its active tasks are
   --  terminated once the subprogram returns.

   procedure Shutdown;
   --  Shutdown every protocol

end System.Garlic.Protocols;

