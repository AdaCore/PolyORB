------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . R E Q U E S T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2018, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Errors.Helper;
with PolyORB.Log;
with PolyORB.ORB.Iface;
with PolyORB.Protocols.Iface;
with PolyORB.Request_QoS;
with PolyORB.Setup;
with PolyORB.Tasking.Threads;

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
      Ignore_Src_Mode :        Boolean;
      Can_Extend      :        Boolean := False);

   procedure Pump_Up_Arguments_By_Position
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean;
      Can_Extend      :        Boolean := False);

   procedure Pump_Up_Arguments_By_Name
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean;
      Can_Extend      :        Boolean := False);

   --  True arguments of direction Direction (or INOUT) from received protocol
   --  arguments list P_Args (either from a request, on server side, or for a
   --  reply, on client side) into A_Args. If Can_Extend is set to True and
   --  Src_Args contains extra arguments that are not required by Dst_Args,
   --  then they are appended.
   --
   --  Each variant of the Pump_Up_Arguments procedure corresponds to a
   --  reconciliation method, according to the identification capabilities of
   --  the personalities.

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Request,
      Name => Request_Access);

   type Request_Completion_Runnable (Req : access Request) is
     new Tasking.Threads.Runnable with null record;
   overriding procedure Run (R : not null access Request_Completion_Runnable);

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
      Req_Flags                  : Flags := Default_Flags;
      Deferred_Arguments_Session : Components.Component_Access := null;
      Identification             : Arguments_Identification :=
        Ident_By_Position;
      Dependent_Binding_Object   : Smart_Pointers.Entity_Ptr := null)
   is
   begin
      pragma Debug (C, O ("Create_Request: enter"));

      Req := new Request;
      Setup_Request (Req                        => Req.all,
                     Target                     => Target,
                     Operation                  => Operation,
                     Arg_List                   => Arg_List,
                     Result                     => Result,
                     Exc_List                   => Exc_List,
                     Req_Flags                  => Req_Flags,
                     Deferred_Arguments_Session => Deferred_Arguments_Session,
                     Identification             => Identification,
                     Dependent_Binding_Object   => Dependent_Binding_Object);

      pragma Debug (C, O ("Create_Request: leave"));
   end Create_Request;

   ---------------------
   -- Destroy_Request --
   ---------------------

   procedure Destroy_Request (Req : in out Request_Access) is
   begin
      --  As a precaution, clear Req.Surrogate so that if a dangling
      --  pointer to Req exists, at least it won't try to access the
      --  servant.

      Req.Surrogate := null;
      Free (Req);
   end Destroy_Request;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Req : in out Request) is
   begin
      Tasking.Mutexes.Create (Req.Upcall_Abortable_Mutex);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Req : in out Request) is
   begin
      PolyORB.Utils.Strings.Free (Req.Operation);
      Annotations.Destroy (Req.Notepad);
      Tasking.Mutexes.Destroy (Req.Upcall_Abortable_Mutex);
   end Finalize;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self         : access Request;
      Invoke_Flags : Flags := 0;
      Timeout      : Duration := 0.0)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Invoke_Flags);
      pragma Warnings (On);

      use PolyORB.ORB;
      use PolyORB.ORB.Iface;
      use PolyORB.Setup;

      Req : constant Request_Access := Self.all'Unchecked_Access;

      R : aliased Request_Completion_Runnable (Self);

   begin
      PolyORB.ORB.Queue_Request_To_Handler (The_ORB,
        Queue_Request'(Request   => Req,
                       Requestor => Req.Requesting_Component));

      --  Execute the ORB until the request is completed

      if Timeout = 0.0 then
         R.Run;
      else
         declare
            use Tasking.Abortables;
            pragma Warnings (Off);
            --  WAG:FSF-4.5.0
            --  Hide warning "AR is not referenced"
            AR      : aliased Abortable'Class :=
              Make_Abortable (Abortable_Tag, R'Access);
            pragma Warnings (On);
            Expired : Boolean := False;
            Error   : Errors.Error_Container;
         begin
            AR.Run_With_Timeout (Timeout, Expired);
            if Expired then
               Throw
                 (Error, Timeout_E,
                  System_Exception_Members'
                    (Minor => 1, Completed => Completed_Maybe));
               Set_Exception (Req.all, Error);
            end if;
         end;
      end if;
   end Invoke;

   -----------------------------------
   -- Pump_Up_Arguments_By_Position --
   -----------------------------------

   procedure Pump_Up_Arguments_By_Position
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Error           : in out Error_Container;
      Ignore_Src_Mode :        Boolean;
      Can_Extend      :        Boolean        := False)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Src_It : Iterator := First (List_Of (Src_Args).all);
      Dst_It : Iterator := First (List_Of (Dst_Args).all);

   begin
      if Same_Entity (Src_Args, Dst_Args) then
         return;
      end if;

      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      --  When Direction is ARG_IN, we are a server and we are pumping
      --  arguments from an incoming request message into the request that will
      --  be processed by the actual application object. In this case, we know
      --  that arguments in Dst_Args have their correct canonical modes and
      --  names. We assume that Src_Args only contain arguments whose actual
      --  mode (as specifid in Dst_Args) is ARG_IN or ARG_INOUT, possibly
      --  without names. If without names, we assume that they are in the order
      --  of Dst_Args.

      --  When direction is ARG_OUT, we are a client and we are pumping up
      --  INOUT and OUT arguments from an incoming reply message into the
      --  request that will be handed back to the client appplication object.
      --  (no return value must be present in Src_Args, only actual arguments).
      --  We assume that Src_Args only contain arguments whose actual mode is
      --  ARG_INOUT or ARG_OUT, possibly without names, and if without names in
      --  the order of Dst_Args.

      --  Note that we cannot rely on the mode indications in Src_Args because
      --  some protocols (eg SOAP) do not set it correcly (more specifically
      --  SOAP does not support deferred unmarshalling, and insist on
      --  unmarshalling Self.Args before Arguments is called. Consequence:
      --  'OUT' mode arguments might be missing in Self.Args, and 'INOUT'
      --  arguments might be marked as 'IN'. Also, there is no guarantee that
      --  the order of arguments is the same in Args and Self.Args.)

      while not Last (Dst_It) loop

         declare
            Dst_Arg : constant Element_Access := Value (Dst_It);
         begin
            if Dst_Arg.Arg_Modes = ARG_INOUT
              or else Dst_Arg.Arg_Modes = Direction
            then

               --  This arguments needs to be pumped up from the Src_Args list.
               --  If Ignore_Arg_Mode is True, we assume that Src contains only
               --  arguments that actually need to be copied, else we check the
               --  arg modes of Src args and copy only those that need to,
               --  according to Direction.

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
                           pragma Debug (C, O ("argument not found"));
                           Throw
                             (Error, Bad_Param_E,
                              System_Exception_Members'
                                (Minor => 1, Completed => Completed_No));
                           return;
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
         --  remaining Src_Args.

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
      Ignore_Src_Mode :        Boolean;
      Can_Extend      :        Boolean := False)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Dst_It : Iterator := First (List_Of (Dst_Args).all);

      Copied_Src_Args : array (1 .. Get_Count (Src_Args)) of Boolean
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

               --  This arguments needs to be pumped up from the Src_Args list.
               --  If Ignore_Arg_Mode is True, we assume that Src contains only
               --  arguments that actually need to be copied, else we check the
               --  arg modes of Src args and copy only those that need to,
               --  according to Direction.

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
                  pragma Debug (C, O ("argument not found"));
                  Throw
                    (Error, Bad_Param_E,
                     System_Exception_Members'
                       (Minor => 1, Completed => Completed_No));
                  return;
               end if;
            end if;
         end;
         Next (Dst_It);
      end loop;

      if Can_Extend then
         --  If Dst_Args is an extensible NV_List, then we append the remaining
         --  Src_Args.

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
      Ignore_Src_Mode :        Boolean;
      Can_Extend      :        Boolean := False)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      function Name_Exists
        (Name : Types.Identifier; From : Iterator) return Boolean;
      --  True if, and only if, the list on which From iterates contains a
      --  NamedValue whose name is Name between the position denoted by From
      --  and the end of the list.

      function Name_Exists
        (Name : Types.Identifier; From : Iterator) return Boolean
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

      Copied_Src_Args : array (1 .. Get_Count (Src_Args)) of Boolean
        := (others => False);
      Src_Idx : Long;
      Src_It : Iterator;
      Copy_Argument : Boolean;
      Identification_By_Name, Identification_By_Position : Boolean := True;
      --  By default, we assume that arguments are identified by both name and
      --  position (this is the ideal case).

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

               --  This arguments needs to be pumped up from the Src_Args list.
               --  If Ignore_Arg_Mode is True, we assume that Src contains only
               --  arguments that actually need to be copied, else we check the
               --  arg modes of Src args and copy only those that need to,
               --  according to Direction.

               Src_It := First (List_Of (Src_Args).all);
               Src_Idx := Copied_Src_Args'First;
               pragma Debug (C, O ("Dst_Arg: "
                                & To_String (Value (Dst_It).Name)));
               loop
                  Copy_Argument := False;
                  --  By default, we will not copy the argument: it is up to
                  --  the algorithm to decide it.

                  if (Ignore_Src_Mode
                      or else Value (Src_It).Arg_Modes = ARG_INOUT
                      or else Value (Src_It).Arg_Modes = Direction)
                    and then Copied_Src_Args (Src_Idx) = False
                  then
                     declare
                        Dst_Arg_Type : constant TypeCode.Object_Ptr :=
                          Get_Unwound_Type (Value (Dst_It).Argument);
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
                                 --  problem if we are identifying arguments by
                                 --  their positions and not by their names,
                                 --  since we then do not consider the names.

                              elsif Identification_By_Name
                                and then Name_Exists
                                (Value (Dst_It).Name, From => Src_It)
                              then
                                 Identification_By_Position := False;
                                 Copy_Argument := False;
                                 --  If the name does not match, but exists,
                                 --  and we are performing identification by
                                 --  name (and possibly identification by
                                 --  position), then we assume that the
                                 --  argument will match by name later and then
                                 --  we are not performing identification by
                                 --  position any more. Thus identification by
                                 --  name has the priority.

                              else
                                 Identification_By_Name := False;
                                 pragma Debug (C, O ("no more ident by name"));
                                 --  If we were identifying the arguments by
                                 --  their names and the name does not match
                                 --  and does not exist in the hash table, then
                                 --  we cannot perform such identification any
                                 --  more.

                                 if Identification_By_Position then
                                    Copy_Argument := True;
                                 else
                                    --  We must identify the arguments by name
                                    --  or by position. Bail out if neither is
                                    --  possible.

                                    pragma Debug (C, O ("dead end"));
                                    Throw
                                      (Error, Bad_TypeCode_E,
                                       System_Exception_Members'
                                         (Minor => 1,
                                          Completed => Completed_No));
                                    return;
                                 end if;
                              end if;
                           end if;
                        else
                           Identification_By_Position := False;
                           pragma Debug (C, O ("no more ident by pos"));

                           --  If we were identifying arguments by their
                           --  positions, the types should have matched (first
                           --  unused src_arg with first unused dst_arg). This
                           --  is not the case, so we are not identifying
                           --  arguments by their positions.

                           if Identification_By_Name then
                              if not Name_Exists
                                (Value (Dst_It).Name, From => Src_It)
                              then
                                 --  If the name does not exist, this means
                                 --  that we will never be able to make this
                                 --  argument match.

                                 pragma Debug (C, O ("name not found"));
                                 Throw
                                   (Error, Bad_Param_E,
                                    System_Exception_Members'
                                      (Minor => 1, Completed => Completed_No));
                                 return;
                              end if;

                              --  Else, the type of Src_Arg does not match
                              --  Dst_Arg, but its name exists in the hash
                              --  table, so we can hope that the argument which
                              --  has the proper name also has the proper type:
                              --  we do nothing but continuing the search
                              --  among Src_Args.

                           else
                              --  We must identify the arguments by name or by
                              --  position. Bail out if neither is possible.

                              Throw
                                (Error, Bad_TypeCode_E,
                                 System_Exception_Members'
                                   (Minor => 1, Completed => Completed_No));
                              pragma Debug (C, O ("by position impossible"));
                              return;
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
                  pragma Debug (C, O ("arg not found"));
                  Throw
                    (Error, Bad_Param_E,
                       System_Exception_Members'
                         (Minor => 1, Completed => Completed_No));
                  return;
               end if;
            end if;
         end;
         Next (Dst_It);
      end loop;

      if Can_Extend then
         --  If Dst_Args is an extensible NV_List, then we append the remaining
         --  Src_Args.

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

   procedure Reset_Request (Request : in out PolyORB.Requests.Request) is
      Null_Any : PolyORB.Any.Any;

   begin
      Request.Completed        := False;
      Request.Arguments_Called := False;
      Request.Exception_Info   := Null_Any;
   end Reset_Request;

   ---------
   -- Run --
   ---------

   overriding procedure Run
     (R : not null access Request_Completion_Runnable)
   is
      use PolyORB.Setup;
   begin
      PolyORB.ORB.Run (The_ORB, R.Req.all'Unchecked_Access, May_Exit => True);
   end Run;

   ---------------
   -- Arguments --
   ---------------

   procedure Arguments
     (Self           : Request_Access;
      Args           : in out Any.NVList.Ref;
      Error          : in out Error_Container;
      Identification : Arguments_Identification := Ident_By_Position;
      Can_Extend     : Boolean := False)
   is
      use Any.NVList;
      use Components;
   begin
      if Self.Arguments_Called
        or else not PolyORB.Any.Is_Empty (Self.Exception_Info)
      then
         pragma Debug (C, O ("Arguments called twice"));
         Throw
           (Error, Bad_Inv_Order_E,
            System_Exception_Members'(Minor => 7, Completed => Completed_No));
         return;
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
            Identification_Method : constant Arguments_Identification :=
              Identification and Self.Args_Ident;
            Ignore_Src_Mode : constant Boolean :=
              Self.Requesting_Component.all in Protocols.Session'Class;
         begin
            if Identification_Method = Ident_By_Position
              or else Identification_Method = Ident_Both
            then
               --  If reconciling arguments by position, and the call comes
               --  from a network connection, assume that only IN arguments are
               --  present, and that the direction indications may be wrong in
               --  Self.Args (because the protocol does not distinguish between
               --  IN and IN OUT arguments). However for a local call, we may
               --  assume that direction indicators are correct, and we must
               --  ensure that we omit all OUT arguments.

               Pump_Up_Arguments_By_Position
                 (Dst_Args        => Args,
                  Src_Args        => Self.Args,
                  Direction       => Any.ARG_IN,
                  Error           => Error,
                  Ignore_Src_Mode => Ignore_Src_Mode,
                  Can_Extend      => Can_Extend);

            elsif Identification_Method = Ident_By_Name then
               Pump_Up_Arguments_By_Name
                 (Dst_Args        => Args,
                  Src_Args        => Self.Args,
                  Direction       => Any.ARG_IN,
                  Error           => Error,
                  Ignore_Src_Mode => Ignore_Src_Mode,
                  Can_Extend      => Can_Extend);

            else
               Pump_Up_Arguments_Unspecified
                 (Dst_Args        => Args,
                  Src_Args        => Self.Args,
                  Direction       => Any.ARG_IN,
                  Error           => Error,
                  Ignore_Src_Mode => Ignore_Src_Mode,
                  Can_Extend      => Can_Extend);
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
         pragma Debug (C, O ("Invalid Set_Result call"));
         Throw
           (Error, Bad_Inv_Order_E,
            System_Exception_Members'(Minor => 8, Completed => Completed_No));
         return;
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

   procedure Set_Exception (Self : in out Request; Error : Error_Container) is
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
      Identification_Method : constant Arguments_Identification :=
        Identification and Self.Args_Ident;
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
      --  Copy back inout and out arguments from Out_Args to Args, so the
      --  requestor finds them where it expects.

      --  XXX If a method has IN and OUT args and R.Args contains only the IN
      --  arguments (and no empty Any's for the OUT ones) what happens?
   end Set_Out_Args;

   -------------------
   -- Setup_Request --
   -------------------

   procedure Setup_Request
     (Req                        : out Request;
      Target                     : References.Ref;
      Operation                  : String;
      Arg_List                   : Any.NVList.Ref;
      Result                     : in out Any.NamedValue;
      Exc_List                   : Any.ExceptionList.Ref :=
        Any.ExceptionList.Nil_Ref;
      Req_Flags                  : Flags := Default_Flags;
      Deferred_Arguments_Session : Components.Component_Access := null;
      Identification             : Arguments_Identification :=
        Ident_By_Position;
      Dependent_Binding_Object   : Smart_Pointers.Entity_Ptr := null)
   is
      use PolyORB.Request_QoS;
      use type Smart_Pointers.Entity_Ptr;

   begin
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
   end Setup_Request;

end PolyORB.Requests;
