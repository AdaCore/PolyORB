------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       M O M A . P R O V I D E R . M E S S A G E _ C O N S U M E R        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Message_Consumer servant.

--  $Id$

with MOMA.Destinations;
with MOMA.Types;
with MOMA.Messages;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Exceptions;

package body MOMA.Provider.Message_Consumer is

   use MOMA.Messages;
   use MOMA.Destinations;
   use MOMA.Types;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Requests;

   package L is
      new PolyORB.Log.Facility_Log ("moma.provider.message_consumer");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --  Actual function implemented by the servant.

   function Get
     (Self       : in PolyORB.References.Ref;
      Message_Id : in MOMA.Types.String)
     return PolyORB.Any.Any;
   --  Return Message_Id message.

   procedure Register_Handler
     (Self        : access Object;
      Handler_Ref :        PolyORB.References.Ref;
      Behavior    :        MOMA.Types.Call_Back_Behavior);
   --  Register a message handler.

   --  Accessors to servant interface.

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description.

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any;
   --  Result part of the interface description.

   ---------
   -- Get --
   ---------

   function Get
     (Self       : in PolyORB.References.Ref;
      Message_Id : in PolyORB.Types.String)
     return PolyORB.Any.Any
   is
      Arg_Name_Mesg : PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("Message");

      Argument_Mesg : PolyORB.Any.Any := PolyORB.Any.To_Any (Message_Id);

      Operation_Name : constant Standard.String := "Get";

      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");

   begin
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   Arg_Name_Mesg,
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => PolyORB.Types.Identifier (Result_Name),
                 Argument  => PolyORB.Any.Get_Empty_Any (TC_MOMA_Message),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Self,
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      --  Retrieve return value.
      return Result.Argument;

   end Get;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Types;

      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));

      if Method = "Get" then
         Add_Item (Result,
                   (Name => To_PolyORB_String ("Message_Id"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));

      elsif Method = "Register_Handler" then
         Add_Item
           (Result,
            (Name => To_PolyORB_String ("Message_Handler"),
             Argument => Get_Empty_Any (MOMA.Destinations.TC_MOMA_Destination),
             Arg_Modes => ARG_IN));

         Add_Item (Result,
                   (Name => To_PolyORB_String ("Behavior"),
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

   ------------------------
   -- Get_Result_Profile --
   ------------------------

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any
   is
      use PolyORB.Any;

   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));

      if Method = "Get" then
         return Get_Empty_Any (TC_MOMA_Message);

      elsif Method = "Register_Handler" then
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
   -- Invoke --
   ------------

   procedure Invoke
     (Self : access Object;
      Req  : in     PolyORB.Requests.Request_Access)
   is
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Exceptions;

      Args  : PolyORB.Any.NVList.Ref;
      It    : Iterator;
      Error : Error_Container;
   begin
      pragma Debug (O ("The server is executing the request:"
                       & PolyORB.Requests.Image (Req.all)));

      PolyORB.Any.NVList.Create (Args);

      if Req.Operation = To_PolyORB_String ("Get") then

         PolyORB.Any.NVList.Add_Item
           (Args,
            (Name => To_PolyORB_String ("Message_Id"),
             Argument => Get_Empty_Any (TypeCode.TC_String),
             Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more contructive

         end if;

         It := First (List_Of (Args).all);
         Set_Result
           (Req, Get (Self.Remote_Ref, From_Any (Value (It).Argument)));
         pragma Debug (O ("Result: " & Image (Req.Result)));

      elsif Req.Operation = To_PolyORB_String ("Register_Handler") then

         --  Register Message call_back handler

         pragma Debug (O ("Register_Handler request"));
         Args := Get_Parameter_Profile (To_Standard_String (Req.Operation));

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
                (From_Any (Behavior.Argument))));
            pragma Debug (O ("Handler registered"));
         end;

      else
         pragma Debug (O ("Unrecognized request "
                          & To_Standard_String (Req.Operation)));
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
      pragma Debug (O ("Registering Message_Handler with " &
                       Call_Back_Behavior'Image (Behavior) & " behavior"));

      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         To_PolyORB_String ("Message_Handler"),
         To_Any (Handler_Dest),
         PolyORB.Any.ARG_IN);

      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         To_PolyORB_String ("Behavior"),
         To_Any (To_PolyORB_String (Call_Back_Behavior'Image (Behavior))),
         PolyORB.Any.ARG_IN);

      Result := (Name      => To_PolyORB_String ("Result"),
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
      pragma Debug (O ("Register_Handler request complete"));

      PolyORB.Requests.Destroy_Request (Request);
      pragma Debug (O ("Register_Handler request destroyed"));
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

end MOMA.Provider.Message_Consumer;
