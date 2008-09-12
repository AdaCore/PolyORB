------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . R E Q U E S T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

--  The Request object.

with Ada.Unchecked_Deallocation;

with PolyORB.Errors.Helper;
with PolyORB.Log;
with PolyORB.ORB.Iface;
with PolyORB.Protocols.Iface;
with PolyORB.Request_QoS;
with PolyORB.Setup;

package body PolyORB.Requests is

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.requests");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Pump_Up_Arguments_Unspecified
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False);

   procedure Pump_Up_Arguments_By_Position
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False);

   procedure Pump_Up_Arguments_By_Name
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False);

   --  True arguments of direction Direction (or INOUT) from received
   --  protocol arguments list P_Args (either from a request, on
   --  server side, or for a reply, on client side) into A_Args.  If
   --  Can_Extend is set to True and Src_Args contains extra arguments
   --  that are not required by Dst_Args, then they are appended.
   --
   --  Each variant of the Pump_Up_Arguments procedure corresponds to
   --  a reconciliation method, according to the identification
   --  capabilities of the personalities.

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Target                     : References.Ref;
      Operation                  : String;
      Arg_List                   : Any.NVList.Ref;
      Result                     : in out Any.NamedValue;
      Exc_List                   : Any.ExceptionList.Ref :=
                                     Any.ExceptionList.Nil_Ref;
      Req                        : out Request_Access;
      Req_Flags                  : Flags :=
                                     Default_Flags;
      Deferred_Arguments_Session : Components.Component_Access :=
                                     null;
      Identification             : Arguments_Identification :=
                                     Ident_By_Position;
      Dependent_Binding_Object   : Smart_Pointers.Entity_Ptr := null)
   is
      use PolyORB.Request_QoS;
      use type Smart_Pointers.Entity_Ptr;

   begin
      pragma Debug (C, O ("Create_Request: enter"));

      Req := new Request;
      Req.Target     := Target;
      Req.Operation  := PolyORB.Utils.Strings."+" (Operation);
      Req.Args       := Arg_List;
      Req.Deferred_Arguments_Session := Deferred_Arguments_Session;
      Req.Result     := Result;
      Req.Result.Arg_Modes := Any.ARG_OUT;
      Req.Exc_List   := Exc_List;
      Req.Args_Ident := Identification;
      Req.Req_Flags  := Req_Flags;

      Set_Request_QoS (Req, Fetch_QoS (Req.Target));

      if Dependent_Binding_Object /= null then
         Smart_Pointers.Set
           (Req.Dependent_Binding_Object, Dependent_Binding_Object);
      end if;
      pragma Debug (C, O ("Create_Request: leave"));
   end Create_Request;

   ---------------------
   -- Destroy_Request --
   ---------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Request, Request_Access);

   procedure Destroy_Request (R : in out Request_Access) is
   begin
      if R /= null then
         PolyORB.Utils.Strings.Free (R.Operation);
         Annotations.Destroy (R.Notepad);
         Free (R);
      end if;
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
      use PolyORB.ORB.Iface;
      use PolyORB.Setup;

   begin
      PolyORB.ORB.Queue_Request_To_Handler (The_ORB,
        Queue_Request'(Request   => Self,
                       Requestor => Self.Requesting_Component));

      --  Execute the ORB until the request is completed

      PolyORB.ORB.Run
        (The_ORB,
         Exit_Condition_T'
           (Condition => Self.Completed'Access,
            Task_Info => Self.Requesting_Task'Access),
         May_Exit => True);
   end Invoke;

   -----------------------------------
   -- Pump_Up_Arguments_By_Position --
   -----------------------------------

   procedure Pump_Up_Arguments_By_Position
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Components;

      Src_It : Iterator := First (List_Of (Src_Args).all);
      Dst_It : Iterator := First (List_Of (Dst_Args).all);

   begin
      if Same_Entity (Src_Args, Dst_Args) then
         return;
      end if;

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

                        --  These MUST be type-compatible!
                        --  Also, if Dst_Arg already provides storage for the
                        --  argument value, we must assign in place using
                        --  Copy_Value (we cannot transfer the value from
                        --  Src_Arg).

                        if Is_Empty (Dst_Arg.Argument) then
                           Move_Any_Value (Dst_Arg.Argument, Src_Arg.Argument);
                        else
                           Copy_Any_Value (Dst_Arg.Argument, Src_Arg.Argument);
                        end if;

                        Next (Src_It);
                        exit;
                     else
                        Next (Src_It);
                        if Last (Src_It) then
                           declare
                              Member : constant System_Exception_Members
                                := (Minor => 1, Completed => Completed_No);
                           begin
                              Throw (Error, Bad_Param_E, Member);
                              pragma Debug (C, O ("arg not found"));
                              return;
                           end;
                        end if;
                     end if;
                  end;

               end loop;
            end if;
            Next (Dst_It);
         end;
      end loop;

      if Can_Extend then
         pragma Debug (C, O ("Appending remaining arguments"));
         --  If Dst_Args is an extensible NV_List, then we append the
         --  remaining Src_Args
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
   end Pump_Up_Arguments_By_Position;

   -------------------------------
   -- Pump_Up_Arguments_By_Name --
   -------------------------------

   procedure Pump_Up_Arguments_By_Name
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False)
   is
      use PolyORB.Components;

      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Dst_It : Iterator := First (List_Of (Dst_Args).all);

      Copied_Src_Args : array (1 .. Long (Get_Count (Src_Args))) of Boolean
        := (others => False);
      Src_Idx : Long;
      Src_It : Iterator;

   begin
      if Same_Entity (Src_Args, Dst_Args) then
         return;
      end if;

      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      --  Same comment as in Pump_Up_Arguments_By_Position

      while not Last (Dst_It) loop
         declare
            Src_Arg_Found : Boolean := False;
         begin
            if Value (Dst_It).Arg_Modes = ARG_INOUT
              or else Value (Dst_It).Arg_Modes = Direction
            then

               --  This arguments needs to be pumped up from the
               --  Src_Args list. If Ignore_Arg_Mode is True,
               --  we assume that Src contains only arguments
               --  that actually need to be copied, else we check
               --  the arg modes of Src args and copy only those
               --  that need to, according to Direction.

               Src_It := First (List_Of (Src_Args).all);
               Src_Idx := Copied_Src_Args'First;
               pragma Debug (C, O ("Dst_Arg: "
                                & To_String (Value (Dst_It).Name)));
               loop
                  if (Ignore_Src_Mode
                      or else Value (Src_It).Arg_Modes = ARG_INOUT
                      or else Value (Src_It).Arg_Modes = Direction)
                    and then Copied_Src_Args (Src_Idx) = False
                  then
                     pragma Debug (C, O ("Src_Arg: "
                                      & To_String (Value (Src_It).Name)));
                     if PolyORB.Any.TypeCode.Equal
                          (Get_Unwound_Type (Value (Dst_It).Argument),
                           Get_Unwound_Type (Value (Src_It).Argument))
                       and then Value (Dst_It).Name = Value (Src_It).Name
                     then
                        pragma Debug (C, O ("Found the argument: copying"));
                        Src_Arg_Found := True;
                        Move_Any_Value (Value (Dst_It).Argument,
                                        Value (Src_It).Argument);
                        Copied_Src_Args (Src_Idx) := True;
                        exit;
                     else
                        Src_Idx := Src_Idx + 1;
                        Next (Src_It);
                        if Last (Src_It) then
                           Src_Arg_Found := False;
                           exit;
                        end if;
                     end if;
                  end if;
               end loop;

               if not Src_Arg_Found then
                  declare
                     Member : constant System_Exception_Members
                       := (Minor => 1, Completed => Completed_No);
                  begin
                     Throw (Error, Bad_Param_E, Member);
                     pragma Debug (C, O ("arg not found"));
                     return;
                  end;
               end if;
            end if;
         end;
         Next (Dst_It);
      end loop;

      if Can_Extend then
         --  If Dst_Args is an extensible NV_List, then we append the
         --  remaining Src_Args

         Src_It := First (List_Of (Src_Args).all);
         Src_Idx := Copied_Src_Args'First;

         pragma Debug (C, O ("Appending remaining arguments"));
         while not Last (Src_It) loop
            if (Ignore_Src_Mode
                or else Value (Src_It).Arg_Modes = ARG_INOUT
                or else Value (Src_It).Arg_Modes = Direction)
              and then Copied_Src_Args (Src_Idx) = False
            then
               Add_Item (Dst_Args, Value (Src_It).all);
            end if;

            Next (Src_It);
            Src_Idx := Src_Idx + 1;
         end loop;
      end if;
   end Pump_Up_Arguments_By_Name;

   -----------------------------------
   -- Pump_Up_Arguments_Unspecified --
   -----------------------------------

   procedure Pump_Up_Arguments_Unspecified
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean        := True;
      Can_Extend      :        Boolean        := False)
   is
      use PolyORB.Components;

      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      function Name_Exists
        (Name : Types.Identifier; From : Iterator)
         return Boolean;
      --  True iff the list on which From iterates contains
      --  a namedvalue whose name is Name between the position
      --  denoted by From and the end of the list.

      function Name_Exists
        (Name : Types.Identifier; From : Iterator)
         return Boolean
      is
         It : Iterator := From;
      begin
         while not Last (It) loop
            if Value (It).Name = Name then
               return True;
            end if;

            Next (It);
         end loop;

         return False;
      end Name_Exists;

      Dst_It : Iterator := First (List_Of (Dst_Args).all);

      Copied_Src_Args : array (1 .. Long (Get_Count (Src_Args))) of Boolean
        := (others => False);
      Src_Idx : Long;
      Src_It : Iterator;
      Copy_Argument : Boolean;
      Identification_By_Name, Identification_By_Position : Boolean := True;
      --  By default, we assume that arguments are identified by both
      --  name and position (this is the ideal case).

   begin
      if Same_Entity (Src_Args, Dst_Args) then
         return;
      end if;

      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      --  Same comments as in Pump_Up_Arguments_By_Position

      while not Last (Dst_It) loop
         declare
            Src_Arg_Found : Boolean := False;
         begin
            if Value (Dst_It).Arg_Modes = ARG_INOUT
              or else Value (Dst_It).Arg_Modes = Direction
            then

               --  This arguments needs to be pumped up from the
               --  Src_Args list. If Ignore_Arg_Mode is True,
               --  we assume that Src contains only arguments
               --  that actually need to be copied, else we check
               --  the arg modes of Src args and copy only those
               --  that need to, according to Direction.

               Src_It := First (List_Of (Src_Args).all);
               Src_Idx := Copied_Src_Args'First;
               pragma Debug (C, O ("Dst_Arg: "
                                & To_String (Value (Dst_It).Name)));
               loop
                  Copy_Argument := False;
                  --  By default, we will not copy the argument: it is
                  --  up to the algorithm to decide it.

                  if (Ignore_Src_Mode
                      or else Value (Src_It).Arg_Modes = ARG_INOUT
                      or else Value (Src_It).Arg_Modes = Direction)
                    and then Copied_Src_Args (Src_Idx) = False
                  then
                     declare
                        Dst_Arg_Type : constant TypeCode.Object_Ptr :=
                                         Get_Unwound_Type
                                           (Value (Dst_It).Argument);
                     begin
                        pragma Debug (C, O ("Src_Arg: "
                                         & To_String (Value (Src_It).Name)));
                        if PolyORB.Any.TypeCode.Equal
                          (Dst_Arg_Type,
                           Get_Unwound_Type (Value (Src_It).Argument))
                        then
                           if Value (Dst_It).Name = Value (Src_It).Name then
                              Copy_Argument := True;
                              --  The arguments match in name and type. Thus
                              --  we can perform the copy, as the arguments
                              --  are identified both by name and position.

                           else
                              if Identification_By_Position
                                and then not Identification_By_Name
                              then
                                 Copy_Argument := True;
                                 --  The name does not match. It is not a
                                 --  problem if we are identifying
                                 --  arguments by their positions and not
                                 --  by their names, since we then do not
                                 --  consider the names.

                              elsif Identification_By_Name
                                and then Name_Exists
                                (Value (Dst_It).Name, From => Src_It)
                              then
                                 Identification_By_Position := False;
                                 Copy_Argument := False;
                                 --  If the name does not match, but
                                 --  exists, and we are performing
                                 --  identification by name (and possibly
                                 --  identification by position), then we
                                 --  assume that the argument will match
                                 --  by name later and then we are not
                                 --  performing identification by
                                 --  position any more. Thus
                                 --  identification by name has the
                                 --  priority.

                              else
                                 Identification_By_Name := False;
                                 pragma Debug (C, O ("no more ident by name"));
                                 --  If we were identifying the arguments
                                 --  by their names and the name does not
                                 --  match and does not exist in the hash
                                 --  table, then we cannot perform such
                                 --  identification any more.

                                 if Identification_By_Position then
                                    Copy_Argument := True;
                                 else
                                    declare
                                       Member : constant
                                         System_Exception_Members
                                         := (Minor => 1,
                                             Completed => Completed_No);
                                    begin
                                       Throw (Error, Bad_TypeCode_E, Member);
                                       pragma Debug (C, O ("dead end"));
                                       return;

                                    --  We must identify the arguments either
                                    --  by their name or their position. If
                                    --  not, this is an error.
                                    end;
                                 end if;
                              end if;
                           end if;
                        else
                           Identification_By_Position := False;
                           pragma Debug (C, O ("no more ident by pos"));
                           --  If we were identifying arguments by their
                           --  positions, the types should have matched
                           --  (first unused src_arg with first unused
                           --  dst_arg). This is not the case, so we are
                           --  not identifying arguments by their
                           --  positions.

                           if Identification_By_Name then
                              if not Name_Exists
                                (Value (Dst_It).Name, From => Src_It)
                              then
                                 --  If the name does not exist, this
                                 --  means that we will never be able to
                                 --  make this argument match.
                                 declare
                                    Member : constant System_Exception_Members
                                      := (Minor => 1,
                                          Completed => Completed_No);
                                 begin
                                    Throw (Error, Bad_Param_E, Member);
                                    pragma Debug (C, O ("name not found"));
                                    return;
                                 end;
                              end if;

                              --  Else, the type of src_arg does not
                              --  match with dst_arg, but its name exists
                              --  in the hash table; so we can hope that
                              --  the argument which has the proper name
                              --  also has the proper type: so we do
                              --  nothing but continuing the search among
                              --  src_args.
                           else
                              declare
                                 Member : constant System_Exception_Members
                                   := (Minor => 1, Completed => Completed_No);
                              begin
                                 Throw (Error, Bad_TypeCode_E, Member);
                                 pragma Debug
                                   (C, O ("by position impossible"));
                                 return;
                                 --  We must identify the arguments either
                                 --  by their name or their position. If
                                 --  not, this is an error.
                              end;
                           end if;
                        end if;
                     end;
                  end if;

                  if Copy_Argument then
                     pragma Debug (C, O ("Found the argument: copying"));
                     Src_Arg_Found := True;
                     Move_Any_Value (Value (Dst_It).Argument,
                                     Value (Src_It).Argument);
                     Copied_Src_Args (Src_Idx) := True;
                     exit;
                  else
                     Src_Idx := Src_Idx + 1;
                     Next (Src_It);
                     if Last (Src_It) then
                        Src_Arg_Found := False;
                        exit;
                     end if;
                  end if;
               end loop;

               if not Src_Arg_Found then
                  declare
                     Member : constant System_Exception_Members
                       := (Minor => 1, Completed => Completed_No);
                  begin
                     Throw (Error, Bad_Param_E, Member);
                     pragma Debug (C, O ("arg not found"));
                     return;
                  end;
               end if;
            end if;
         end;
         Next (Dst_It);
      end loop;

      if Can_Extend then
         --  If dst_args is an extensible NV_List, then we append the
         --  remaining Src_Args

         Src_It := First (List_Of (Src_Args).all);
         Src_Idx := Copied_Src_Args'First;

         pragma Debug (C, O ("Appending remaining arguments"));
         while not Last (Src_It) loop
            if (Ignore_Src_Mode
                or else Value (Src_It).Arg_Modes = ARG_INOUT
                or else Value (Src_It).Arg_Modes = Direction)
              and then Copied_Src_Args (Src_Idx) = False
            then
               Add_Item (Dst_Args, Value (Src_It).all);
            end if;

            Next (Src_It);
            Src_Idx := Src_Idx + 1;
         end loop;
      end if;
   end Pump_Up_Arguments_Unspecified;

   -------------------
   -- Reset_Request --
   -------------------

   procedure Reset_Request (Request : PolyORB.Requests.Request_Access) is
      Null_Any : PolyORB.Any.Any;

   begin
      Request.Completed := False;
      Request.Arguments_Called := False;
      Request.Exception_Info := Null_Any;
   end Reset_Request;

   ---------------
   -- Arguments --
   ---------------

   procedure Arguments
     (Self           :        Request_Access;
      Args           : in out Any.NVList.Ref;
      Error          : in out Error_Container;
      Identification :        Arguments_Identification := Ident_By_Position;
      Can_Extend     :        Boolean := False)
   is
      use Any.NVList;
      use Components;

   begin
      if Self.Arguments_Called
        or else not PolyORB.Any.Is_Empty (Self.Exception_Info)
      then
         declare
            Member : constant System_Exception_Members
              := (Minor => 7, Completed => Completed_No);
         begin
            pragma Debug (C, O ("Arguments called twice"));
            Throw (Error, Bad_Inv_Order_E, Member);
            return;
         end;
      end if;
      Self.Arguments_Called := True;

      if Is_Nil (Self.Args) then
         pragma Assert (Self.Deferred_Arguments_Session /= null);
         declare
            use Protocols.Iface;

            Reply : constant Components.Message'Class
              := Components.Emit
              (Self.Deferred_Arguments_Session,
               Unmarshall_Arguments'
               (Args => Args));
         begin
            pragma Assert (Reply in Unmarshalled_Arguments
                             or else Reply in Arguments_Error);
            if Reply in Unmarshalled_Arguments then
               pragma Debug (C, O ("Unmarshalled deferred arguments"));
               Args := Unmarshalled_Arguments (Reply).Args;
               Self.Args := Args;
            else
               pragma Debug (C, O ("Unmarshalling deferred arguments error"));
               Error := Arguments_Error (Reply).Error;
            end if;
         end;
         Self.Deferred_Arguments_Session := null;

      else
         pragma Assert (Self.Deferred_Arguments_Session = null);
         pragma Debug (C, O ("in Arguments: " & Image (Self.Args)));

         declare
            Identification_Method : constant Arguments_Identification
              := Identification and Self.Args_Ident;

         begin
            if Identification_Method = Ident_By_Position
              or else Identification_Method = Ident_Both
            then
               Pump_Up_Arguments_By_Position
                 (Dst_Args   => Args,
                  Src_Args   => Self.Args,
                  Direction  => Any.ARG_IN,
                  Error      => Error,
                  Can_Extend => Can_Extend);
            elsif Identification_Method = Ident_By_Name then
               Pump_Up_Arguments_By_Name
                 (Dst_Args   => Args,
                  Src_Args   => Self.Args,
                  Direction  => Any.ARG_IN,
                  Error      => Error,
                  Can_Extend => Can_Extend);
            else
               Pump_Up_Arguments_Unspecified
                 (Dst_Args   => Args,
                  Src_Args   => Self.Args,
                  Direction  => Any.ARG_IN,
                  Error      => Error,
                  Can_Extend => Can_Extend);
            end if;
         end;
      end if;

      Self.Out_Args := Args;
   end Arguments;

   -----------
   -- Image --
   -----------

   function Image (Req : Request) return String is
   begin
      return "Operation: "
        & Req.Operation.all
        & " on object "
        & References.Image (Req.Target)
        & " with arguments "
        & Any.NVList.Image (Req.Args);
   end Image;

   ----------------
   -- Set_Result --
   ----------------

   procedure Set_Result
     (Self  : Request_Access;
      Val   : Any.Any;
      Error : in out Error_Container)
   is
      use PolyORB.Any;
   begin
      if not Self.Arguments_Called
        or else Self.Set_Result_Called
        or else not PolyORB.Any.Is_Empty (Self.Exception_Info)
      then
         declare
            Member : constant System_Exception_Members :=
                       (Minor => 8, Completed => Completed_No);
         begin
            pragma Debug (C, O ("Invalid Set_Result call"));
            Throw (Error, Bad_Inv_Order_E, Member);
            return;
         end;
      end if;

      Self.Set_Result_Called := True;
      if Is_Empty (Self.Result.Argument) then
         Set_Type (Self.Result.Argument, Get_Type_Obj (Val));
         Move_Any_Value (Self.Result.Argument, Val);
      else
         Copy_Any_Value (Self.Result.Argument, Val);
      end if;
   end Set_Result;

   procedure Set_Result (Self : Request_Access; Val : Any.Any) is
      Error : Error_Container;
   begin
      Set_Result (Self, Val, Error);
      pragma Assert (not Is_Error (Error));
   end Set_Result;

   -------------------
   -- Set_Exception --
   -------------------

   procedure Set_Exception (Self : Request_Access; Error : Error_Container) is
   begin
      Self.Exception_Info := PolyORB.Errors.Helper.Error_To_Any (Error);
   end Set_Exception;

   ------------------
   -- Set_Out_Args --
   ------------------

   procedure Set_Out_Args
     (Self           : Request_Access;
      Error          : in out Error_Container;
      Identification : Arguments_Identification := Ident_By_Position)
   is
      Identification_Method : constant Arguments_Identification
        := Identification and Self.Args_Ident;
   begin
      if Identification_Method = Ident_By_Position
        or else Identification_Method = Ident_Both
      then
         Pump_Up_Arguments_By_Position
           (Dst_Args        => Self.Args,
            Src_Args        => Self.Out_Args,
            Direction       => PolyORB.Any.ARG_OUT,
            Ignore_Src_Mode => False,
            Error           => Error);
      elsif Identification_Method = Ident_By_Name then
         Pump_Up_Arguments_By_Name
           (Dst_Args        => Self.Args,
            Src_Args        => Self.Out_Args,
            Direction       => PolyORB.Any.ARG_OUT,
            Ignore_Src_Mode => False,
            Error           => Error);
      else
         Pump_Up_Arguments_Unspecified
           (Dst_Args        => Self.Args,
            Src_Args        => Self.Out_Args,
            Direction       => PolyORB.Any.ARG_OUT,
            Ignore_Src_Mode => False,
            Error           => Error);
      end if;
      --  Copy back inout and out arguments from Out_Args
      --  to Args, so the requestor finds them where
      --  it expects.

      --  XXX If a method has IN and OUT args and R.Args
      --  contains only the IN arguments (and no empty
      --  Any's for the OUT ones) what happens?
   end Set_Out_Args;

end PolyORB.Requests;
