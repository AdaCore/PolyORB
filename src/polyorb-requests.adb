------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . R E Q U E S T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  The Request object.

--  $Id$

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
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

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     Any.Context.Ref;
      Operation : in     Operation_Id;
      Arg_List  : in     Any.NVList.Ref;
      Result    : in out Any.NamedValue;
      --  Exc_List  : in     ExceptionList.Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access;
      --  Req_Flags : in     Flags
      Deferred_Arguments_Session : in Components.Component_Access := null
     )
   is
      Res : constant Request_Access := new Request;
      Result_Any : PolyORB.Any.Any := Any.Get_By_Ref (Result.Argument);
   begin
      Res.Target    := Target;
      Res.Operation := To_PolyORB_String (Operation);
      Res.Args      := Arg_List;
      Res.Deferred_Arguments_Session := Deferred_Arguments_Session;
      Res.Result    :=
        (Name      => Result.Name,
         Argument  => Result_Any,
         Arg_Modes => Result.Arg_Modes);

      Req := Res;
   end Create_Request;

   procedure Invoke (Self : Request_Access)
   is
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.Setup;

   begin
      PolyORB.ORB.Queue_Request_To_Handler
        (The_ORB.Tasking_Policy, The_ORB,
         Queue_Request'
         (Request   => Self,
          Requestor => null));
      --  XXX Only synchronous requests are supported!

      --  Execute the ORB until the request is completed.
      ORB.Run
        (The_ORB, Exit_Condition_T'
         (Condition => Self.Completed'Access,
          Task_Info => Self.Requesting_Task'Access),
         May_Poll => True);

   end Invoke;

   procedure Arguments
     (Self : Request_Access;
      Args : in out Any.NVList.Ref)
   is
      use Any.NVList;
      use Components;

   begin
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
            Args := Unmarshalled_Arguments (Reply).Args;
            Self.Args := Args;
         end;
         Self.Deferred_Arguments_Session := null;
      else
         pragma Assert
           (Self.Deferred_Arguments_Session = null
            and then not Is_Nil (Self.Args));

         pragma Debug (O ("in Arguments: " & Image (Self.Args)));

         declare
            use PolyORB.Any;
            use PolyORB.Any.NVList.Internals;
            use PolyORB.Any.NVList.Internals.NV_Sequence;
            P_Arg_Index : Integer := 1;
            --  Index in Self.Args (protocol layer arguments)
         begin
            for A_Arg_Index in 1 .. Get_Count (Args) loop
               --  Index in Args (application layer arguments)
               declare
                  A_Arg : NamedValue
                    := Element_Of (List_Of (Args).all,
                                   Integer (A_Arg_Index));
                  P_Arg : constant NamedValue
                    := Element_Of (List_Of (Self.Args).all, P_Arg_Index);
               begin
                  if A_Arg.Arg_Modes = ARG_IN
                    or else A_Arg.Arg_Modes = ARG_INOUT
                  then
                     --  Application says it wants this arg as
                     --  input: take it from P_Args.
                     Copy_Any_Value (A_Arg.Argument, P_Arg.Argument);
                     --  These MUST be type-compatible!

                     P_Arg_Index := P_Arg_Index + 1;
                  else
                     --  An ARG_OUT argument

                     if P_Arg.Arg_Modes = ARG_OUT then
                        --  present: skip it.
                        P_Arg_Index := P_Arg_Index + 1;
                     end if;
                  end if;
               end;
            end loop;
         end;

         --  XXX This is not the whole story.

         --  In an ideal world, Self.Args (from the Protocol layer)
         --  and Args (from the application layer) are mode-conformant.
         --  Unfortunately, some protocols (eg SOAP) do not support
         --  deferred unmarshalling, and insist on unmarshalling Self.Args
         --  before Arguments is called. Consequence: 'OUT' mode arguments
         --  might be missing in Self.Args, and 'INOUT' arguments might
         --  be marked as 'IN'. Also, there is no guarantee that the order
         --  of arguments is the same in Args and Self.Args. An attempt
         --  should be made to reconcile argument names and argument types
         --  (tricky. See how Ada compilers do parameter reconciliation with
         --  support for both named and positional parameter associations.)

         --  Here we do our best by assuming that P_Args are exactly conformant
         --  with A_Args, possibly with ARG_OUT arguments missing.

      end if;
   end Arguments;

   function Image (Req : Request) return String
   is
      S1 : constant String
        := "Operation: " & To_Standard_String (Req.Operation)
        & " on object " & References.Image (Req.Target);
   begin
      declare
         S2 : constant String := Any.NVList.Image (Req.Args);
      begin
         return S1 & " with arguments " & S2;
      end;
   exception
      when others =>
         --  Could not render arguments.
         return S1 & " with non-representable arguments";
   end Image;

end PolyORB.Requests;
