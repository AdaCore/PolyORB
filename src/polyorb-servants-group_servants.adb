------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E R V A N T S . G R O U P _ S E R V A N T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with Ada.Tags;

with PolyORB.Log;
with PolyORB.ORB.Iface;
with PolyORB.Protocols.Iface;
with PolyORB.Requests;
with PolyORB.Servants.Iface;
with PolyORB.Setup;
with PolyORB.Types;

package body PolyORB.Servants.Group_Servants is

   use PolyORB.Any.NVList;
   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Setup;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.servants.group_servants");
   procedure O (Message : Standard.String; Level : Log.Log_Level := Log.Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package TPL renames Target_List_Package;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   function Handle_Unmarshall_Arguments
     (Self : access Group_Servant;
      Msg  :        Components.Message'Class)
     return Components.Message'Class;
   --  Dispatch arguments between targets

   function Handle_Unmarshall_Arguments
     (Self : access Group_Servant;
      Msg  :        Components.Message'Class)
      return Components.Message'Class
   is
      use PolyORB.Protocols.Iface;

   begin
      pragma Assert (Msg in Unmarshall_Arguments);
      Enter (Self.Mutex);

      case Self.State is
         when Not_Ready =>
            Leave (Self.Mutex);

            raise Program_Error;

         when Wait_First =>
            --  Wait for first argument ask

            pragma Debug (C, O ("Try to unmarshall arguments"));
            pragma Assert (Self.Args_Src /= null);

            --  Check that request is oneway

            declare
               use PolyORB.Any;
               use PolyORB.Any.NVList.Internals.NV_Lists;

               It : PolyORB.Any.NVList.Internals.NV_Lists.Iterator
                 := First (PolyORB.Any.NVList.Internals.List_Of
                           (Unmarshall_Arguments (Msg).Args).all);
            begin
               while not Last (It) loop
                  if Value (It).Arg_Modes = ARG_OUT
                    or else Value (It).Arg_Modes = ARG_INOUT
                  then
                     Leave (Self.Mutex);
                     raise Constraint_Error;
                  end if;

                  Next (It);
               end loop;
            end;

            --  Unmarshall arguments from protocol stack

            declare
               use PolyORB.Any.NVList.Internals.NV_Lists;

               Reply : constant Message'Class := Emit (Self.Args_Src, Msg);
               Req_Args : Ref renames Unmarshalled_Arguments (Reply).Args;
               It : PolyORB.Any.NVList.Internals.NV_Lists.Iterator;

            begin
               pragma Assert (Reply in Unmarshalled_Arguments
                                or else Reply in Arguments_Error);

               if Reply in Unmarshalled_Arguments then
                  pragma Debug (C, O ("Arguments unmarshalled, copying"));

                  Create (Self.Args);
                  It := First (PolyORB.Any.NVList.Internals.List_Of
                                 (Req_Args).all);
                  while not Last (It) loop
                     Add_Item (Self.Args, Value (It).all);
                     Next (It);
                  end loop;

                  --  Replace the Argument component with a by-value
                  --  copy of the original one (thus ensuring that the
                  --  value remains valid even after exiting the
                  --  current scope).

                  It := First (PolyORB.Any.NVList.Internals.List_Of
                                 (Self.Args).all);
                  while not Last (It) loop
                     Value (It).all.Argument
                       := PolyORB.Any.Copy_Any (Value (It).all.Argument);
                     Next (It);
                  end loop;

                  pragma Debug
                    (C, O ("Send arguments to first servant in group"));

                  Self.State := Wait_Other;
                  Leave (Self.Mutex);

                  --  Send result

                  return Unmarshalled_Arguments'(Args => Req_Args);

               else
                  pragma Debug (C, O ("Arguments unmarshalling error"));

                  --  Reply in Arguments_Error

                  Self.Error := Arguments_Error (Reply).Error;

                  Self.State := Wait_Other;

                  --  Copy error and send result

                  declare
                     Aux : Error_Container;
                  begin
                     Throw (Aux, Self.Error.Kind, Self.Error.Member.all);
                     Leave (Self.Mutex);
                     return Arguments_Error'(Error => Aux);
                  end;
               end if;
            end;

         when Wait_Other =>
            --  Copy arguments (or error) and send it

            if not Found (Self.Error) then
               pragma Debug (C, O ("Get previously unmarshalled arguments"));
               declare
                  use PolyORB.Any;
                  use PolyORB.Any.NVList.Internals.NV_Lists;

                  Req_Args : Ref renames Unmarshall_Arguments (Msg).Args;
                  It1 : PolyORB.Any.NVList.Internals.NV_Lists.Iterator;
                  It2 : PolyORB.Any.NVList.Internals.NV_Lists.Iterator;
               begin
                  pragma Assert (Get_Count (Self.Args) = Get_Count (Req_Args));

                  It1 := First (PolyORB.Any.NVList.Internals.List_Of
                                  (Self.Args).all);
                  It2 := First (PolyORB.Any.NVList.Internals.List_Of
                                  (Req_Args).all);

                  while not Last (It1) loop
                     pragma Assert (Value (It1).Name = Value (It2).Name);
                     pragma Assert (Value (It1).Arg_Modes
                                    = Value (It2).Arg_Modes);

                     Copy_Any_Value (Value (It2).Argument,
                                     Value (It1).Argument);
                     Next (It1);
                     Next (It2);
                  end loop;

                  Leave (Self.Mutex);
                  return Unmarshalled_Arguments'(Args => Req_Args);
               end;
            else
               pragma Debug (C, O ("Copy unmarshalling arguments error"));

               declare
                  Aux : Error_Container;
               begin
                  Throw (Aux, Self.Error.Kind, Self.Error.Member.all);
                  Leave (Self.Mutex);
                  return Arguments_Error'(Error => Aux);
               end;
            end if;
      end case;
   end Handle_Unmarshall_Arguments;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access Group_Servant;
      Msg  : Components.Message'Class)
      return Components.Message'Class
   is
      use PolyORB.Requests;
      use PolyORB.ORB;
      use PolyORB.Any;
      use PolyORB.ORB.Iface;
      use PolyORB.Servants.Iface;
      use Unsigned_Long_Flags;

      Request : Request_Access;
      It : TPL.Iterator;

      Res : PolyORB.Components.Null_Message;

   begin
      pragma Assert (Msg in Execute_Request);

      Request := Execute_Request (Msg).Req;
      if TPL.Length (Self.Target_List) = 0 then
         pragma Debug (C, O ("Request received in empty group !!!",
                          PolyORB.Log.Warning));
         return Executed_Request'(Req => Request);
      end if;

      --  Initialize argument proxy

      Enter (Self.Group_Lock);
      Enter (Self.Mutex);

      pragma Debug (C, O ("Request received on group servant : "
                       & PolyORB.Objects.Image (Self.Oid.all)));

      pragma Assert (Is_Nil (Request.Args));

      --  Check if request is oneway

      if not Is_Set (Sync_With_Transport, Request.Req_Flags) then
         Leave (Self.Group_Lock);
         Leave (Self.Mutex);

         raise Constraint_Error;
      end if;

      if Self.State = Wait_Other then
         Free (Self.Args);
         Catch (Self.Error);
      end if;

      Self.Counter := 0;
      Self.State := Wait_First;
      Self.Args_Src := Request.Deferred_Arguments_Session;

      It := TPL.First (Self.Target_List);

      --  Create requests

      while not TPL.Last (It) loop
         declare
            Req  : Request_Access;
            Args : Ref;

         begin
            pragma Debug (C, O ("Forward to : "
                             & PolyORB.References.Image (TPL.Value (It).all)));

            Create_Request
              (Target                     =>
                 PolyORB.References.Ref'(TPL.Value (It).all),
               Operation                  =>
                 Request.Operation.all,
               Arg_List                   => Args,
               Result                     => Request.Result,
               Deferred_Arguments_Session =>
                 PolyORB.Components.Component_Access (Self),
               Req                        => Req,
               Req_Flags                  => Request.Req_Flags);
            --  XXX Notepad is not copied, neither are QoS parameters ..

            --  Requeue request to ORB

            Queue_Request_To_Handler (The_ORB,
              Queue_Request'
                (Request   => Req,
                 Requestor => PolyORB.Components.Component_Access (Self)));

            pragma Debug (C, O ("Request sent"));
            TPL.Next (It);
         end;
      end loop;

      Leave (Self.Mutex);

      pragma Debug (C, O ("Request dispatched to all servants in group"));

      return Res;
   end Execute_Servant;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Self : access Group_Servant;
      Msg  :        Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Servants.Iface;
      use PolyORB.Protocols.Iface;

      Res : PolyORB.Components.Null_Message;

   begin
      pragma Debug (C, O ("Handling message of type "
                       & Ada.Tags.External_Tag (Msg'Tag)));

      if Msg in Unmarshall_Arguments then
         return Handle_Unmarshall_Arguments (Self, Msg);

      elsif Msg in Executed_Request then
         Enter (Self.Mutex);

         declare
            use PolyORB.Requests;
            Req : Request_Access := Executed_Request (Msg).Req;

         begin
            pragma Debug (C, O ("Destroy request"));
            Destroy_Request (Req);
         end;

         Self.Counter := Self.Counter + 1;

         if Self.Counter = TPL.Length (Self.Target_List) then
            Leave (Self.Group_Lock);
         end if;

         Leave (Self.Mutex);

         return Res;

      else
         --  Dispatch

         return PolyORB.Servants.Handle_Message
           (Servant (Self.all)'Access, Msg);
      end if;
   end Handle_Message;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self : access Group_Servant;
      Ref  :        PolyORB.References.Ref) is
   begin
      pragma Debug (C, O ("Register on group servant : "
                       & PolyORB.Objects.Image (Self.Oid.all)));
      pragma Debug (C, O ("Ref : " & PolyORB.References.Image (Ref)));

      Enter (Self.Group_Lock);

      TPL.Append (Self.Target_List, Ref);
      pragma Debug (C, O ("Group size:" & TPL.Length (Self.Target_List)'Img));

      Leave (Self.Group_Lock);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Self : access Group_Servant;
      Ref  :        PolyORB.References.Ref)
   is
      use PolyORB.References;

   begin
      pragma Debug (C, O ("Unregister on group servant: "
                       & PolyORB.Objects.Image (Self.Oid.all)));
      pragma Debug (C, O ("Ref : " & PolyORB.References.Image (Ref)));

      Enter (Self.Group_Lock);

      TPL.Remove (Self.Target_List, Ref);
      pragma Debug (C, O ("Group size:" & TPL.Length (Self.Target_List)'Img));

      Leave (Self.Group_Lock);
   end Unregister;

   ---------------------------
   -- Destroy_Group_Servant --
   ---------------------------

   procedure Destroy_Group_Servant
     (Group : in out PolyORB.Servants.Servant_Access)
   is
      GS : constant Group_Servant_Access := Group_Servant (Group.all)'Access;

   begin
      TPL.Deallocate (GS.Target_List);
      Destroy (GS.Mutex);
      Destroy (GS.Group_Lock);
      Group := null;
   end Destroy_Group_Servant;

   --------------------------
   -- Create_Group_Servant --
   --------------------------

   function Create_Group_Servant
     (Oid : Object_Id_Access)
     return PolyORB.Servants.Servant_Access
   is
      GS : constant Group_Servant_Access := new Group_Servant;

   begin
      pragma Debug (C, O ("Create group servant : "
                       & PolyORB.Objects.Image (Oid.all)));
      GS.Oid := Oid;
      Create (GS.Mutex);
      Create (GS.Group_Lock);

      return PolyORB.Servants.Servant_Access (GS);
   end Create_Group_Servant;

   -------------------------
   -- Get_Group_Object_Id --
   -------------------------

   procedure Get_Group_Object_Id
     (Group :        PolyORB.Servants.Servant_Access;
      Oid   :    out Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      if not (Group.all in Group_Servant) then
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         return;
      end if;

      Oid := Group_Servant_Access (Group).Oid;
   end Get_Group_Object_Id;

   ----------------------
   -- Get_Group_Length --
   ----------------------

   procedure Get_Group_Length
     (Group :        PolyORB.Servants.Servant_Access;
      L     :    out Natural;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      if not (Group.all in Group_Servant) then
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         pragma Warnings (Off);  --  "L" not set before return
         return;
         pragma Warnings (On);
      end if;

      L := TPL.Length (Group_Servant_Access (Group).Target_List);
   end Get_Group_Length;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Group :        PolyORB.Servants.Servant_Access;
      Ref   :        PolyORB.References.Ref;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      if not (Group.all in Group_Servant) then
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         return;
      end if;

      Register (Group_Servant_Access (Group), Ref);
   end Associate;

   ------------------
   -- Disassociate --
   ------------------

   procedure Disassociate
     (Group :        PolyORB.Servants.Servant_Access;
      Ref   :        PolyORB.References.Ref;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      if not (Group.all in Group_Servant) then
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         return;
      end if;

      Unregister (Group_Servant_Access (Group), Ref);
   end Disassociate;

   --------------------
   -- Group Iterator --
   --------------------

   -----------
   -- First --
   -----------

   procedure First
     (Group :        PolyORB.Servants.Servant_Access;
      It    :    out Iterator;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      if not (Group.all in Group_Servant) then
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         return;
      end if;
      It.It := TPL.First (Group_Servant_Access (Group).Target_List);
   end First;

   -----------
   -- Value --
   -----------

   function Value (It : Iterator) return PolyORB.References.Ref is
   begin
      return TPL.Value (It.It).all;
   end Value;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Iterator) is
   begin
      TPL.Next (It.It);
   end Next;

   ----------
   -- Last --
   ----------

   function Last (It : Iterator) return Boolean is
   begin
      return TPL.Last (It.It);
   end Last;

end PolyORB.Servants.Group_Servants;
