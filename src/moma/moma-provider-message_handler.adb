------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . P R O V I D E R . M E S S A G E _ H A N D L E R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Message_Handler servant.

--  $Id$

with MOMA.Messages;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Requests;

package body MOMA.Provider.Message_Handler is

   use MOMA.Message_Handlers;

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Requests;

   package L is
     new PolyORB.Log.Facility_Log ("moma.provider.message_handler");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --  Actual function implemented by the servant.

   procedure Handle (Self    : access Object;
                     Message : PolyORB.Any.Any);
   --  Execute the Handler procedure.
   --  Called when receiving a Handle request.

   procedure Notify (Self : access Object);
   --  Execute the Notifier procedure.
   --  Called when receiving a Notify request.

   --  Accessors to servant interface.

   function Get_Parameter_Profile (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description.

   function Get_Result_Profile (Method : String)
     return PolyORB.Any.Any;
   --  Result part of the interface description.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : access Object;
      MOMA_Message_Handler : MOMA.Message_Handlers.Message_Handler_Acc) is
   begin
      Self.MOMA_Message_Handler := MOMA_Message_Handler;
   end Initialize;

   ------------
   -- Invoke --
   ------------

   procedure Invoke (Self : access Object;
                     Req  : in     PolyORB.Requests.Request_Access)
   is
      Args        : PolyORB.Any.NVList.Ref;
      Operation   : constant String := To_Standard_String (Req.Operation);
   begin
      pragma Debug (O ("The message handler is executing the request:"
                    & PolyORB.Requests.Image (Req.all)));

      PolyORB.Any.NVList.Create (Args);

      Args := Get_Parameter_Profile (Operation);
      PolyORB.Requests.Arguments (Req, Args);

      if Req.Operation = To_PolyORB_String ("Notify") then
         Notify (Self);

      elsif Operation = "Handle" then
         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Message        : PolyORB.Any.Any :=
              NV_Sequence.Element_Of (Args_Sequence.all, 1).Argument;
         begin
            Handle (Self, Message);
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

   ------------------------
   -- Get_Result_Profile --
   ------------------------

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any is
   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));

      if Method = "Handle" or else Method = "Notify" then
         return Get_Empty_Any (TypeCode.TC_Void);

      else
         raise Program_Error;
      end if;

   end Get_Result_Profile;

   -------------
   -- If_Desc --
   -------------

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description is
   begin
      return
        (PP_Desc => Get_Parameter_Profile'Access,
         RP_Desc => Get_Result_Profile'Access);
   end If_Desc;

   ------------
   -- Handle --
   ------------

   procedure Handle (Self    : access Object;
                     Message : PolyORB.Any.Any)
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

end MOMA.Provider.Message_Handler;
