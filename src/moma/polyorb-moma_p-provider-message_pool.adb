------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.MOMA_P.PROVIDER.MESSAGE_POOL                    --
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

--  Message_Pool servant.

with MOMA.Destinations;
with MOMA.Messages;

with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.MOMA_P.Provider.Message_Pool is

   use MOMA.Messages;

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("moma.provider.message_pool");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --  Actual functions implemented by the servant.

   procedure Publish
     (Self    : access Object;
      Message : PolyORB.Any.Any);

   function Get
     (Self       : access Object;
      Message_Id : MOMA.Types.String)
     return PolyORB.Any.Any;

   procedure Register_Handler
     (Self        : access Object;
      Handler_Ref :        PolyORB.References.Ref;
      Behavior    :        MOMA.Types.Call_Back_Behavior);

   --  Accessors to servant interface.

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description.

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

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access)
   is
      Args : PolyORB.Any.NVList.Ref;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      Error : Error_Container;
   begin
      pragma Debug (C, O ("The server is executing the request:"
                       & PolyORB.Requests.Image (Req.all)));

      Create (Args);

      if Req.Operation.all = "Publish" then

         --  Publish

         Add_Item (Args,
                   (Name      => Message_S,
                    Argument  => Get_Empty_Any (TC_MOMA_Message),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more contructive

         end if;

         Publish (Self, Value (First (List_Of (Args).all)).Argument);

      elsif Req.Operation.all = "Get" then

         --  Get

         Add_Item (Args,
                   (Name => Message_Id_S,
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more contructive

         end if;

         Req.Result.Argument := Get
           (Self,
            MOMA.Types.String
            (PolyORB.Types.String'
             (PolyORB.Any.From_Any
              (Value (First (List_Of (Args).all)).Argument))));
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
            It : Iterator := First (List_Of (Args).all);

            Handler_Dest, Behavior : Element_Access;
         begin
            Handler_Dest := Value (It);
            Next (It);
            Behavior     := Value (It);

            Register_Handler
              (Self,
               MOMA.Destinations.Get_Ref
               (MOMA.Destinations.From_Any (Handler_Dest.Argument)),
               MOMA.Types.Call_Back_Behavior'Value
               (MOMA.Types.To_Standard_String
                (MOMA.Types.From_Any (Behavior.Argument))));

            pragma Debug (C, O ("Registered message handler"));
         end;
      else
         pragma Debug (C, O ("Unrecognized request " & Req.Operation.all));
         raise Program_Error;
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

      if Method = "Publish" then
         Add_Item (Result,
                   (Name => Message_S,
                    Argument => Get_Empty_Any (TC_MOMA_Message),
                    Arg_Modes => ARG_IN));

      elsif Method = "Get" then
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

      else
         raise Program_Error;
      end if;

      return Result;
   end Get_Parameter_Profile;

   ------------------------------
   -- Servant actual functions --
   ------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : access Object;
      Info :        MOMA.Types.Message_Pool) is
   begin
      Self.Pool := Info;
      PolyORB.MOMA_P.Provider.Warehouse.Set_Persistence
        (Self.W,
         MOMA.Types.Get_Persistence (Info));

   end Initialize;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Self    : access Object;
      Message : PolyORB.Any.Any)
   is
      Temp : constant String := Integer'Image (Self.Message_Id);
      Key  : constant String := "M" & Temp (2 .. Temp'Last);
      --  Dummy Key construction, should be analyzed from message

      Rcvd_Message : constant MOMA.Messages.Message'Class :=
                       From_Any (Message);
      Id : constant String :=
             MOMA.Types.To_Standard_String (Get_Message_Id (Rcvd_Message));

   begin
      if Self.Behavior = Handle
        and then not PolyORB.References.Is_Nil (Self.Message_Handler)
      then
         --  Send the message to the Message Call_Back Handler.
         --  Do not store the message locally.
         pragma Debug (C, O ("Got new message " & Image (Message)
                          & " with Id " & Key & ", forwarding to Message_"
                          & "Handler with Handle request"));
         declare
            Request     : PolyORB.Requests.Request_Access;
            Arg_List    : PolyORB.Any.NVList.Ref;
            Result      : PolyORB.Any.NamedValue;
         begin
            PolyORB.Any.NVList.Create (Arg_List);

            PolyORB.Any.NVList.Add_Item (Arg_List,
                                         Message_S,
                                         Message,
                                         PolyORB.Any.ARG_IN);
            Result :=
              (Name      => Result_S,
               Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
               Arg_Modes => 0);

            PolyORB.Requests.Create_Request
              (Target    => Self.Message_Handler,
               Operation => "Handle",
               Arg_List  => Arg_List,
               Result    => Result,
               Req       => Request);

            PolyORB.Requests.Invoke (Request);

            PolyORB.Requests.Destroy_Request (Request);
         end;

      else

         if Id = "moma" then
            pragma Debug (C, O ("Got new message " & Image (Message)
                             & " with Id " & Key));
            Self.Message_Id := Self.Message_Id + 1;
            PolyORB.MOMA_P.Provider.Warehouse.Register (Self.W, Key, Message);

         else
            pragma Debug (C, O ("Got new message " & Image (Message)
                             & " with Id " & Id));
            PolyORB.MOMA_P.Provider.Warehouse.Register (Self.W, Id, Message);
         end if;

         if Self.Behavior = Notify
           and then not PolyORB.References.Is_Nil (Self.Message_Handler)
         then
            pragma Debug (C, O ("Forwarding to Message_Handler"
                             & " with Notify request"));
            --  Notify call_back Handler.
            --  The Message is stored locally.
            declare
               Request     : PolyORB.Requests.Request_Access;
               Arg_List    : PolyORB.Any.NVList.Ref;
               Result      : PolyORB.Any.NamedValue;
            begin
               PolyORB.Any.NVList.Create (Arg_List);

               Result :=
                 (Name      => Result_S,
                  Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                  Arg_Modes => 0);

               PolyORB.Requests.Create_Request
                 (Target    => Self.Message_Handler,
                  Operation => "Notify",
                  Arg_List  => Arg_List,
                  Result    => Result,
                  Req       => Request);

               PolyORB.Requests.Invoke (Request);

               PolyORB.Requests.Destroy_Request (Request);
            end;
         end if;

      end if;
   end Publish;

   ---------
   -- Get --
   ---------

   function Get
     (Self       : access Object;
      Message_Id : MOMA.Types.String)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any;
      Temp : constant String := Integer'Image (Self.Last_Read_Id);
      Key  : constant String := "M" & Temp (2 .. Temp'Last);
      Id : constant String := MOMA.Types.To_Standard_String (Message_Id);

   begin
      if Id = "" then
         Result := PolyORB.MOMA_P.Provider.Warehouse.Lookup (Self.W, Key);
         PolyORB.MOMA_P.Provider.Warehouse.Unregister (Self.W, Key);
         Self.Last_Read_Id := Self.Last_Read_Id + 1;

         pragma Debug (C, O ("Sending back message " & Image (Result)
                          & " with id " & Key));
      else
         Result := PolyORB.MOMA_P.Provider.Warehouse.Lookup (Self.W, Key);
         pragma Debug (C, O ("Sending back message " & Image (Result)
                          & " with id " & Key));

      end if;

      return Result;
   end Get;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Self        : access Object;
      Handler_Ref :        PolyORB.References.Ref;
      Behavior    :        MOMA.Types.Call_Back_Behavior) is
   begin
      Self.Message_Handler := Handler_Ref;
      Self.Behavior := Behavior;
   end Register_Handler;

end PolyORB.MOMA_P.Provider.Message_Pool;
