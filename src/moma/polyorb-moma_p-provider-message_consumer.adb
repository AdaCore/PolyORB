------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.MOMA_P.PROVIDER.MESSAGE_CONSUMER                  --
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

--  Message_Consumer servant

with MOMA.Destinations;
with MOMA.Types;
with MOMA.Messages;

with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.QoS;
with PolyORB.Request_QoS;
with PolyORB.Types;

package body PolyORB.MOMA_P.Provider.Message_Consumer is

   use MOMA.Messages;
   use MOMA.Destinations;
   use MOMA.Types;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log ("moma.provider.message_consumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --  Actual function implemented by the servant

   function Get
     (Self       : PolyORB.References.Ref;
      Message_Id : MOMA.Types.String;
      QoS_Params : PolyORB.QoS.QoS_Parameters)
     return PolyORB.Any.Any;
   --  Return Message_Id message

   procedure Register_Handler
     (Self        : access Object;
      Handler_Ref :        PolyORB.References.Ref;
      Behavior    :        MOMA.Types.Call_Back_Behavior);
   --  Register a message handler

   --  Accessors to servant interface

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description

   Message_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Message");

   Message_Id_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Message_Id");

   Message_Handler_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Message_Handler");

   Behavior_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Behavior");

   Result_S : constant PolyORB.Types.Identifier
     := To_PolyORB_String ("Result");

   ---------
   -- Get --
   ---------

   function Get
     (Self       : PolyORB.References.Ref;
      Message_Id : MOMA.Types.String;
      QoS_Params : PolyORB.QoS.QoS_Parameters)
     return PolyORB.Any.Any
   is
      Argument_Mesg : constant PolyORB.Any.Any :=
                        PolyORB.Any.To_Any (PolyORB.Types.String (Message_Id));

      Operation_Name : constant Standard.String := "Get";

      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;

   begin
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   Message_S,
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => Result_S,
                 Argument  => PolyORB.Any.Get_Empty_Any (TC_MOMA_Message),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Self,
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Request_QoS.Set_Request_QoS (Request.all, QoS_Params);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      return Result.Argument;
   end Get;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      use PolyORB.Any.NVList;

      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (C, O ("Parameter profile for " & Method & " requested."));

      if Method = "Get" then
         Add_Item (Result,
                   (Name => Message_Id_S,
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));

      elsif Method = "Register_Handler" then
         Add_Item
           (Result,
            (Name => Message_Handler_S,
             Argument => Get_Empty_Any (MOMA.Destinations.TC_MOMA_Destination),
             Arg_Modes => ARG_IN));

         Add_Item (Result,
                   (Name => Behavior_S,
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
         --  XXX should use an enum type !
      else
         raise Program_Error;

      end if;

      return Result;
   end Get_Parameter_Profile;

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

   procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access)
   is
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      Args  : PolyORB.Any.NVList.Ref;
      It    : Iterator;
      Error : Error_Container;

      QoS_Params  : PolyORB.QoS.QoS_Parameters;
   begin
      pragma Debug (C, O ("The server is executing the request:"
                       & PolyORB.Requests.Image (Req.all)));

      PolyORB.Any.NVList.Create (Args);

      if Req.Operation.all = "Get" then

         PolyORB.Any.NVList.Add_Item
           (Args,
            (Name => Message_Id_S,
             Argument => Get_Empty_Any (TypeCode.TC_String),
             Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more contructive

         end if;

         QoS_Params (PolyORB.QoS.Static_Priority) :=
           PolyORB.Request_QoS.Extract_Request_Parameter
           (PolyORB.QoS.Static_Priority, Req.all);

         It := First (List_Of (Args).all);
         Set_Result
           (Req,
            Get (Self.Remote_Ref,
                 MOMA.Types.String
                 (PolyORB.Types.String'(PolyORB.Any.From_Any
                                        (Value (It).Argument))),
                 QoS_Params));
         pragma Debug (C, O ("Result: " & Image (Req.Result)));

      elsif Req.Operation.all = "Register_Handler" then

         --  Register Message call_back handler

         pragma Debug (C, O ("Register_Handler request"));
         Args := Get_Parameter_Profile (Req.Operation.all);

         PolyORB.Requests.Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more contructive

         end if;

         declare
            Handler_Dest, Behavior : Element_Access;
         begin

            It := First (List_Of (Args).all);
            Handler_Dest := Value (It);
            Next (It);
            Behavior := Value (It);
            Register_Handler
              (Self,
               MOMA.Destinations.Get_Ref
               (MOMA.Destinations.From_Any (Handler_Dest.Argument)),
              MOMA.Types.Call_Back_Behavior'Value
               (MOMA.Types.To_Standard_String
                (MOMA.Types.From_Any (Behavior.Argument))));
            pragma Debug (C, O ("Handler registered"));
         end;

      else
         pragma Debug (C, O ("Unrecognized request "
                          & Req.Operation.all));
         null;
      end if;
   end Invoke;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Self        : access Object;
      Handler_Ref : PolyORB.References.Ref;
      Behavior    : MOMA.Types.Call_Back_Behavior)
   is
      Request      : PolyORB.Requests.Request_Access;
      Arg_List     : PolyORB.Any.NVList.Ref;
      Result       : PolyORB.Any.NamedValue;
      Handler_Dest : constant MOMA.Destinations.Destination :=
        MOMA.Destinations.Create_Destination
        (To_PolyORB_String (""), Handler_Ref);
   begin
      pragma Debug (C, O ("Registering Message_Handler with " &
                       Call_Back_Behavior'Image (Behavior) & " behavior"));

      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Message_Handler_S,
         To_Any (Handler_Dest),
         PolyORB.Any.ARG_IN);

      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Behavior_S,
         PolyORB.Any.To_Any
         (To_PolyORB_String (Call_Back_Behavior'Image (Behavior))),
         PolyORB.Any.ARG_IN);

      Result := (Name      => Result_S,
                 Argument  => PolyORB.Any.Get_Empty_Any
                 (TypeCode.TC_Void),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Self.Remote_Ref,
         Operation => "Register_Handler",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      pragma Debug (C, O ("Register_Handler request complete"));

      PolyORB.Requests.Destroy_Request (Request);
      pragma Debug (C, O ("Register_Handler request destroyed"));
   end Register_Handler;

   --------------------
   -- Set_Remote_Ref --
   --------------------

   procedure Set_Remote_Ref
     (Self : in out Object;
      Ref  :        PolyORB.References.Ref) is
   begin
      Self.Remote_Ref := Ref;
   end Set_Remote_Ref;

end PolyORB.MOMA_P.Provider.Message_Consumer;
