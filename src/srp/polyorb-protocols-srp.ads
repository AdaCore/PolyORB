------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P R O T O C O L S . S R P                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  A protocol similar to the HTTP protocol
--  SRP : Simple Request Protocol (from M. Friess report)

--  SRP is inspired by HTTP. A SRP request has the following form :
--  method object_Id?arg1=val1&arg2=val2
--
--  The following rules apply :
--  Argument name is not significant, only the order is.
--
--  Following types can be marshalled and unmarshalled :
--   byte, boolean, short, long, unsigned short, unsigned long

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Buffers;
with PolyORB.Objects;
with PolyORB.Types;
with PolyORB.Utils.SRP;

package PolyORB.Protocols.SRP is
   pragma Elaborate_Body;

   use PolyORB.Buffers;
   use PolyORB.Objects;
   use PolyORB.Utils.SRP;

   SRP_Error : exception;

   type SRP_Protocol is new Protocol with private;

   --  Message types that can be used with SRP
   type Msg_Type is
     (Req,
      Reply);

   type Reply_Status_Type is
     (Ack,
      Error);

   type SRP_Session is new Session with private;

   procedure Create
     (Proto   : access SRP_Protocol;
      Session : out Filter_Access);

   procedure Connect (S : access SRP_Session);
   --  Do nothing.

   procedure Invoke_Request
     (S : access SRP_Session;
      R : Request_Access;
      P : access Binding_Data.Profile_Type'Class);
   --  Do nothing.

   procedure Abort_Request (S : access SRP_Session; R :  Request_Access);
   --  Do nothing.

   procedure Send_Reply (S : access SRP_Session; R :  Request_Access);
   --  Send a reply to the user.

   procedure Handle_Connect_Indication (S : access SRP_Session);
   --  Send a greeting banner to user.

   procedure Handle_Connect_Confirmation (S : access SRP_Session);
   --  Setup client dialog.

   procedure Handle_Data_Indication
     (S           : access SRP_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container);
   --  Handle data received from user

   procedure Handle_Disconnect
     (S : access SRP_Session; Error : Errors.Error_Container);
   --  Handle disconnection from user

   procedure Handle_Unmarshall_Arguments
     (Ses   : access SRP_Session;
      Args  : in out Any.NVList.Ref;
      Error : in out Errors.Error_Container);

   procedure Handle_Flush (S : access SRP_Session);

   procedure Unmarshall_Request_Message
     (Buffer : access Buffer_Type;
      Oid    : access Object_Id;
      Method : access Types.String);
   --  Get from the buffer the Object_Id and the Method to be called

   procedure Unmarshall_Args
     (Buffer : access Buffer_Type;
      Args   : in out Any.NVList.Ref);
   --  Unmarshall the arguments from Buffer in Args
   --  Args must be an arg list with empty any(s), but with their type set

   procedure Unmarshall
     (Args : in out Any.NVList.Ref; SRP_Info : Split_SRP);
   --  Get the values stored in Info_SRP and unmarshall them in Args
   --  Attention: Args already should contain the types
   --  (cf. Obj_Adapters.Get_Empty_Arg_List)

private
   type SRP_Protocol is new Protocol with null record;

   type SRP_Session is new Session with record
      Buffer_In          : Buffers.Buffer_Access;
      Buffer_Out         : Buffers.Buffer_Access;
      SRP_Info           : Split_SRP;
   end record;

end PolyORB.Protocols.SRP;
