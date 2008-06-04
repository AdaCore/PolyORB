------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.MOMA_P.PROVIDER.MESSAGE_HANDLER                  --
--                                                                          --
--                                 B o d y                                  --
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

--  Message_Handler servant.

with MOMA.Messages;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Errors;

package body PolyORB.MOMA_P.Provider.Message_Handler is

   use MOMA.Message_Handlers;

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Requests;

   package L is
     new PolyORB.Log.Facility_Log ("moma.provider.message_handler");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   --  Actual function implemented by the servant.

   procedure Handle
     (Self    : access Object;
      Message : PolyORB.Any.Any);
   --  Execute the Handler procedure.
   --  Called when receiving a Handle request.

   procedure Notify (Self : access Object);
   --  Execute the Notifier procedure.
   --  Called when receiving a Notify request.

   --  Accessors to servant interface.

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : access Object;
      MOMA_Message_Handler :        MOMA.Message_Handlers.Message_Handler_Acc)
   is
   begin
      Self.MOMA_Message_Handler := MOMA_Message_Handler;
   end Initialize;

   ------------
   -- Invoke --
   ------------

   procedure Invoke (Self : access Object;
                     Req  : PolyORB.Requests.Request_Access)
   is
      use PolyORB.Errors;

      Args        : PolyORB.Any.NVList.Ref;
      Operation   : String renames Req.Operation.all;
      Error       : Error_Container;
   begin
      pragma Debug (O ("The message handler is executing the request:"
                    & PolyORB.Requests.Image (Req.all)));

      Args := Get_Parameter_Profile (Operation);
      PolyORB.Requests.Arguments (Req, Args, Error);

      if Found (Error) then
         raise Program_Error;
         --  XXX We should do something more contructive
      end if;

      if Operation = "Notify" then
         Notify (Self);

      elsif Operation = "Handle" then
         declare
            use PolyORB.Any.NVList.Internals;
            use PolyORB.Any.NVList.Internals.NV_Lists;
         begin
            Handle (Self, Value (First (List_Of (Args).all)).Argument);
         end;
      end if;
   end Invoke;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));

      if Method = "Notify" then
         null;

      elsif Method = "Handle" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Message"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Messages.TC_MOMA_Message),
              Arg_Modes => PolyORB.Any.ARG_IN));

      else
         raise Program_Error;
      end if;

      return Result;
   end Get_Parameter_Profile;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Self    : access Object;
      Message :        PolyORB.Any.Any)
   is
      Rcvd_Message : constant MOMA.Messages.Message'Class
         := MOMA.Messages.From_Any (Message);
      Handler_Procedure : constant MOMA.Message_Handlers.Handler
         := Get_Handler (Self.MOMA_Message_Handler);
   begin
      if Handler_Procedure /= null then
         Handler_Procedure.all (Self.MOMA_Message_Handler, Rcvd_Message);
      end if;
   end Handle;

   ------------
   -- Notify --
   ------------

   procedure Notify (Self : access Object)
   is
      Notifier_Procedure : constant MOMA.Message_Handlers.Notifier
         := Get_Notifier (Self.MOMA_Message_Handler);
   begin
      if Notifier_Procedure /= null then
         Notifier_Procedure.all (Self.MOMA_Message_Handler);
      end if;
   end Notify;

end PolyORB.MOMA_P.Provider.Message_Handler;
