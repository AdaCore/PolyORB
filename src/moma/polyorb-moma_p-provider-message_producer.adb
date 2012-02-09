------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.MOMA_P.PROVIDER.MESSAGE_PRODUCER                  --
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

pragma Ada_2005;

--  Message_Producer servant

with MOMA.Messages;

with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.QoS;
with PolyORB.Request_QoS;
with PolyORB.Types;

package body PolyORB.MOMA_P.Provider.Message_Producer is

   use MOMA.Messages;

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Types;

   package L is
     new PolyORB.Log.Facility_Log ("moma.provider.message_producer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --  Actual function implemented by the servant

   procedure Publish
     (Self    : PolyORB.References.Ref;
      Message : PolyORB.Any.Any;
      QoS_Params : PolyORB.QoS.QoS_Parameters);
   --  Publish a message

   Message_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Message");
   Result_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Result");

   --------------------
   -- Get_Remote_Ref --
   --------------------

   function Get_Remote_Ref
     (Self : Object)
     return PolyORB.References.Ref is
   begin
      return Self.Remote_Ref;
   end Get_Remote_Ref;

   ------------
   -- Invoke --
   ------------

   overriding procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access)
   is
      use PolyORB.Errors;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Args  : PolyORB.Any.NVList.Ref;
      Error : Error_Container;
      QoS_Params : PolyORB.QoS.QoS_Parameters;

   begin
      pragma Debug (C, O ("The server is executing the request:"
                    & PolyORB.Requests.Image (Req.all)));

      Create (Args);

      if Req.all.Operation.all = "Publish" then

         --  Publish

         Add_Item (Args,
                   (Name => Message_S,
                    Argument => Get_Empty_Any (TC_MOMA_Message),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more contructive

         end if;

         QoS_Params (PolyORB.QoS.Static_Priority) :=
           PolyORB.Request_QoS.Extract_Request_Parameter
           (PolyORB.QoS.Static_Priority, Req.all);

         Publish
           (Self.Remote_Ref,
            Value (First (List_Of (Args).all)).Argument,
            QoS_Params);
      end if;
   end Invoke;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Self    : PolyORB.References.Ref;
      Message : PolyORB.Any.Any;
      QoS_Params : PolyORB.QoS.QoS_Parameters)
   is
      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;

   begin
      pragma Debug (C, O ("Publishing Message " & Image (Message)));

      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   Message_S,
                                   Message,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => Result_S,
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Self,
         Operation => "Publish",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Request_QoS.Set_Request_QoS (Request.all, QoS_Params);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      pragma Debug (C, O ("Message published"));
   end Publish;

   --------------------
   -- Set_Remote_Ref --
   --------------------

   procedure Set_Remote_Ref
     (Self : in out Object;
      Ref  :        PolyORB.References.Ref) is
   begin
      Self.Remote_Ref := Ref;
   end Set_Remote_Ref;

end PolyORB.MOMA_P.Provider.Message_Producer;
