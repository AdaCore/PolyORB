------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . R E Q U E S T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  The Request object.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.ORB.Interface;
with PolyORB.Protocols.Interface;
with PolyORB.Setup;

package body PolyORB.Requests is

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.requests");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     Any.Context.Ref;
      Operation : in     String;
      Arg_List  : in     Any.NVList.Ref;
      Result    : in out Any.NamedValue;
      Exc_List  : in     Any.ExceptionList.Ref
        := Any.ExceptionList.Nil_Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access;
      Req_Flags : in     Flags := 0;
      Deferred_Arguments_Session : in Components.Component_Access := null
     )
   is
      Res : constant Request_Access := new Request;
   begin
      pragma Debug (O ("Creating request"));

      Res.Target    := Target;
      Res.Operation := To_PolyORB_String (Operation);
      Res.Args      := Arg_List;
      Res.Deferred_Arguments_Session := Deferred_Arguments_Session;
      Res.Result    := Result;
      Res.Result.Arg_Modes := Any.ARG_OUT;
      Res.Exc_List  := Exc_List;

      if Req_Flags = 0 then
         Res.Req_Flags := Default_Flags;
      else
         Res.Req_Flags := Req_Flags;
      end if;

      Req := Res;
   end Create_Request;

   ---------------------
   -- Destroy_Request --
   ---------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Request, Request_Access);

   procedure Destroy_Request
     (R : in out Request_Access) is
   begin
      Annotations.Destroy (R.Notepad);
      Free (R);
   end Destroy_Request;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self         : Request_Access;
      Invoke_Flags : Flags := 0)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Invoke_Flags);
      pragma Warnings (On);

      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.Setup;

   begin
      PolyORB.ORB.Queue_Request_To_Handler
        (The_ORB.Tasking_Policy,
         The_ORB,
         Queue_Request'
         (Request   => Self,
          Requestor => Self.Requesting_Component));
      --   Requestor => null));

      --  Execute the ORB until the request is completed.
      PolyORB.ORB.Run
        (The_ORB,
         Exit_Condition_T'
         (Condition => Self.Completed'Access,
          Task_Info => Self.Requesting_Task'Access),
         May_Poll => True);

   end Invoke;

   -----------------------
   -- Pump_Up_Arguments --
   -----------------------

   procedure Pump_Up_Arguments
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False)
   is
      use PolyORB.Components;

      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Src_It : Iterator := First (List_Of (Src_Args).all);
      Dst_It : Iterator := First (List_Of (Dst_Args).all);

   begin
      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      --  When Direction is ARG_IN, we are a server and we
      --  are pumping arguments from an incoming request message
      --  into the request that will be processed by the
      --  actual application object. In this case, we know
      --  that arguments in Dst_Args have their correct canonical
      --  modes and names. We assume that Src_Args only contain
      --  arguments whose actual mode (as specifid in Dst_Args) is
      --  ARG_IN or ARG_INOUT, possibly without names. If without
      --  names, we assume that they are in the order of Dst_Args.

      --  When direction is ARG_OUT, we are a client and
      --  we are pumping up INOUT and OUT arguments from an
      --  incoming reply message into the request that will be
      --  handed back to the client appplication object.
      --  (no return value must be present in Src_Args, only
      --  actual arguments). We assue that Src_Args only contain
      --  arguments whose actual mode is ARG_INOUT or ARG_OUT,
      --  possibly without names, and if without names in the
      --  order of Dst_Args.

      --  Note that we cannot rely on the mode indications in
      --  Src_Args because some protocols (eg SOAP) do not
      --  set it correcly (more specifically SOAP does not support
      --  deferred unmarshalling, and insist on unmarshalling Self.Args
      --  before Arguments is called. Consequence: 'OUT' mode arguments
      --  might be missing in Self.Args, and 'INOUT' arguments might
      --  be marked as 'IN'. Also, there is no guarantee that the order
      --  of arguments is the same in Args and Self.Args.)

      --  XXX actually for now we do not check names at all:
      --  we skip Src_Args with the wrong direction and then assume
      --  strict positional association.
      --  If we were brave guys, we should attempt to reconcile
      --  argument names and argument types (Tricky: see how Ada
      --  compilers do parameter reconciliation with support for both
      --  named and positional parameter associations.)

      while not Last (Dst_It) loop

         declare
            Dst_Arg : constant Element_Access := Value (Dst_It);

         begin
            if Dst_Arg.Arg_Modes = ARG_INOUT
              or else Dst_Arg.Arg_Modes = Direction
            then

               --  This arguments needs to be pumped up from the
               --  Src_Args list. If Ignore_Arg_Mode is True,
               --  we assume that Src contains only arguments
               --  that actually need to be copied, else we check
               --  the arg modes of Src args and copy only those
               --  that need to, according to Direction.

               loop
                  declare
                     Src_Arg : constant Element_Access := Value (Src_It);
                  begin
                     if Ignore_Src_Mode
                       or else Src_Arg.Arg_Modes = ARG_INOUT
                       or else Src_Arg.Arg_Modes = Direction
                     then
                        Copy_Any_Value (Dst_Arg.Argument, Src_Arg.Argument);
                        Next (Src_It);
                        --  These MUST be type-compatible!
                        exit;
                     else
                        Next (Src_It);
                        if Last (Src_It) then
                           raise Program_Error;
                        end if;
                     end if;
                  end;

               end loop;
            end if;
            Next (Dst_It);
         end;
      end loop;

      if Can_Extend then
         pragma Debug (O ("Appending remaining arguments"));

         --  If dst_args is an extensible NV_List, then we append the
         --  remaining src_args

         while not Last (Src_It) loop
            if Ignore_Src_Mode
              or else Value (Src_It).Arg_Modes = ARG_INOUT
              or else Value (Src_It).Arg_Modes = Direction
            then
               Add_Item (Dst_Args, Value (Src_It).all);
            end if;

            Next (Src_It);
         end loop;
      end if;

   end Pump_Up_Arguments;

   ---------------
   -- Arguments --
   ---------------

   procedure Arguments
     (Self : Request_Access;
      Args : in out Any.NVList.Ref) is
   begin
      Arguments (Self, Args, False);
   end Arguments;

   procedure Arguments
     (Self :              Request_Access;
      Args :       in out Any.NVList.Ref;
      Can_Extend :        Boolean)
   is
      use Any.NVList;
      use Components;

   begin
      if Self.Arguments_Called then
         pragma Debug (O ("Arguments called twice"));
         raise Program_Error;
      end if;
      Self.Arguments_Called := True;

      if Is_Nil (Self.Args) then
         pragma Assert (Self.Deferred_Arguments_Session /= null);
         declare
            use Protocols.Interface;

            Reply : constant Components.Message'Class
              := Components.Emit
              (Self.Deferred_Arguments_Session,
               Unmarshall_Arguments'
               (Args => Args));
         begin
            pragma Assert (Reply in Unmarshalled_Arguments);
            pragma Debug (O ("Unmarshalled deferred arguments"));
            Args := Unmarshalled_Arguments (Reply).Args;
            Self.Args := Args;
         end;
         Self.Deferred_Arguments_Session := null;

      else
         pragma Assert
           (Self.Deferred_Arguments_Session = null
            and then not Is_Nil (Self.Args));

         pragma Debug (O ("in Arguments: " & Image (Self.Args)));

         Pump_Up_Arguments
           (Dst_Args   => Args,
            Src_Args   => Self.Args,
            Direction  => Any.ARG_IN,
            Can_Extend => Can_Extend);
      end if;

      Self.Out_Args := Args;
   end Arguments;

   -----------
   -- Image --
   -----------

   function Image
     (Req : Request)
     return String
   is
      S1 : constant String
        := "Operation: "
        & To_Standard_String (Req.Operation)
        & " on object "
        & References.Image (Req.Target);
   begin
      declare
         S2 : constant String := Any.NVList.Image (Req.Args);
      begin
         return S1 & " with arguments " & S2;
      end;

   exception
      when others =>
         --  For some kinds of Any's, bugs in the respective
         --  Image procedures may trigger exceptions. In such
         --  cases, we do not want to fail here because we are
         --  only computing an informational, debugging-oriented
         --  message. Consequently, we return a placeholder
         --  value rather than propagating the exception.

         return S1 & " with non-representable arguments";
   end Image;

   ----------------
   -- Set_Result --
   ----------------

   procedure Set_Result
     (Self : Request_Access;
      Val  : Any.Any)
   is
      use PolyORB.Any;

   begin
      if TypeCode.Kind (Get_Type (Self.Result.Argument)) = Tk_Void then
         Self.Result :=
           (Name      => PolyORB.Types.To_PolyORB_String ("result"),
            Argument  => Val,
            Arg_Modes => ARG_OUT);
      else
         Copy_Any_Value (Self.Result.Argument, Val);
      end if;
   end Set_Result;

   ------------------
   -- Set_Out_Args --
   ------------------

   procedure Set_Out_Args
     (Self : Request_Access) is
   begin
      Pump_Up_Arguments
        (Dst_Args        => Self.Args,
         Src_Args        => Self.Out_Args,
         Direction       => PolyORB.Any.ARG_OUT,
         Ignore_Src_Mode => False);
      --  Copy back inout and out arguments from Out_Args
      --  to Args, so the requestor finds them where
      --  it expects.

      --  XXX If a method has IN and OUT args and R.Args
      --  contains only the IN arguments (and no empty
      --  Any's for the OUT ones) what happens?
   end Set_Out_Args;

end PolyORB.Requests;
