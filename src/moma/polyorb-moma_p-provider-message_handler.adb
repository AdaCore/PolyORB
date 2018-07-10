------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.MOMA_P.PROVIDER.MESSAGE_HANDLER                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

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

   overriding procedure Invoke (Self : access Object;
                     Req  : PolyORB.Requests.Request_Access)
   is
      use PolyORB.Errors;

      Args        : PolyORB.Any.NVList.Ref;
      Operation   : String renames Req.Operation.all;
      Error       : Error_Container;
   begin
      pragma Debug (C, O ("The message handler is executing the request:"
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
      pragma Debug (C, O ("Parameter profile for " & Method & " requested."));

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
