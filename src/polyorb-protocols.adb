------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P R O T O C O L S                     --
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

--  Support for object method invocation protocols.

with Ada.Tags;

with PolyORB.Filters.Iface;
with PolyORB.If_Descriptors;
with PolyORB.Log;
with PolyORB.Protocols.Iface;
with PolyORB.Servants.Iface;

package body PolyORB.Protocols is

   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.Protocols.Iface;
   use PolyORB.Servants.Iface;
   use Unsigned_Long_Flags;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (S     : access Session;
      Args  : in out Any.NVList.Ref;
      Error : in out Errors.Error_Container)
   is
   begin
      raise Program_Error;
      --  By default: no support for deferred arguments unmarshalling.
      --  Concrete Session implementations may override this operation
      --  to provide this functionality.
   end Handle_Unmarshall_Arguments;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (Sess : not null access Session;
      S    : Components.Message'Class) return Components.Message'Class
   is
      use PolyORB.Errors;

      Nothing : Components.Null_Message;
      Req     : Request_Access;
      Error   : Errors.Error_Container;

   begin
      pragma Debug
        (C, O ("Handling message of type "
            & Ada.Tags.External_Tag (S'Tag)));
      if S in Connect_Indication then
         Handle_Connect_Indication (Session_Access (Sess));

      elsif S in Connect_Confirmation then
         Handle_Connect_Confirmation (Session_Access (Sess));

      elsif S in Disconnect_Indication then
         Handle_Disconnect
           (Session_Access (Sess), Disconnect_Indication (S).Error);

      elsif S in Data_Indication then
         Handle_Data_Indication
           (Session_Access (Sess), Data_Indication (S).Data_Amount, Error);

         if Found (Error) then
            return Filter_Error'(Error => Error);
         end if;

      elsif S in Unmarshall_Arguments then
         declare
            Args : PolyORB.Any.NVList.Ref
              := Unmarshall_Arguments (S).Args;
         begin
            Handle_Unmarshall_Arguments
              (Session_Access (Sess), Args, Error);

            if Found (Error) then
               return Arguments_Error'(Error => Error);
            else
               return Unmarshalled_Arguments'(Args => Args);
            end if;
         end;

      elsif S in Flush then
         Handle_Flush (Session_Access (Sess));

      elsif S in Set_Server then
         Sess.Server                   := Set_Server (S).Server;
         Sess.Dependent_Binding_Object := Set_Server (S).Binding_Object;

      elsif S in Execute_Request then

         Req := Execute_Request (S).Req;

         declare
            use type Binding_Data.Profile_Access;

            Req_Flags : constant Flags := Req.Req_Flags;
            --  Req may be destroyed as soon as we have called Invoke_Request
            --  below, so we need to take a copy of its flags in advance.

         begin
            pragma Assert (Execute_Request (S).Pro /= null);

            if Req.Deferred_Arguments_Session /= null then

               --  This session object participates in a proxy construct: now
               --  is the last place we can determine the signature of the
               --  called method in order to translate the request. As we do
               --  not have the actual servant on the local node, we need
               --  another way of retrieving an interface description (i.e. a
               --  parameter and result profile). This is typically achieved by
               --  looking up the target interface in an interface repository.
               --  In PolyORB, such operations are abstracted by the
               --  If_Descriptor interface.

               declare
                  use PolyORB.If_Descriptors;

                  Desc : If_Descriptor_Access renames Default_If_Descriptor;
                  --  Delegate the decision and lookup process to the default
                  --  interface descriptor objet.

                  Args : constant Any.NVList.Ref :=
                    Get_Empty_Arg_List (Desc, Req.Target, Req.Operation.all);

                  Reply : constant Components.Message'Class :=
                    Components.Emit (Req.Deferred_Arguments_Session,
                      Unmarshall_Arguments'(Args => Args));

               begin
                  pragma Assert (Reply in Unmarshalled_Arguments
                                 or else Reply in Arguments_Error);
                  if Reply in Unmarshalled_Arguments then
                     pragma Debug (C, O ("Unmarshalled deferred arguments"));
                     Req.Args := Unmarshalled_Arguments (Reply).Args;
                     Req.Result.Argument := Get_Empty_Result
                       (Desc, Req.Target, Req.Operation.all);

                     Req.Deferred_Arguments_Session := null;
                     pragma Debug
                       (C, O ("Proxying request: " & Image (Req.all)));

                  else
                     pragma Debug
                       (C, O ("Unmarshall deferred arguments error"));
                     Set_Exception (Req.all, Arguments_Error (Reply).Error);

                     --  Free data associated to Arguments_Error (Reply).Error

                     declare
                        Error : Error_Container :=
                          Arguments_Error (Reply).Error;
                     begin
                        Catch (Error);
                     end;
                  end if;
               end;
            end if;

            if Found (Error) then
               return Executed_Request'(Req => Req);
            end if;

            Invoke_Request
              (Session_Access (Sess), Req, Execute_Request (S).Pro);

            --  At this point, the request has been sent to the server:
            --  We cannot rely on Req still existing, since if it is a two-way
            --  request, it may have been completed and destroyed. If it is a
            --  one-way, however, we are responsible for signalling that it has
            --  been completed.

            if False
              or else Is_Set (Sync_With_Transport, Req_Flags)
              or else Is_Set (Sync_Call_Back,      Req_Flags)
            then
               pragma Debug (C, O ("Completed Sync_With_Transport"));
               Req.Completed := True;
               return Executed_Request'(Req => Req);
            end if;
         end;

      elsif S in Servants.Iface.Abort_Request then
         Abort_Request
           (Session_Access (Sess),
            Servants.Iface.Abort_Request (S).Req);
         return Null_Message'(null record);

      elsif S in Executed_Request then
         declare
            Req : Request_Access := Executed_Request (S).Req;
         begin
            if Req.Deferred_Arguments_Session /= null then

               --  The request has been aborted before being fully processed.
               --  Flush the session's data and restore the session to its
               --  initial state, waiting for requests.

               Emit_No_Reply
                 (Component_Access (Sess),
                  Protocols.Iface.Flush'(Message with null record));
            end if;

            --  Send a reply if one is expected (per sync scope) and the
            --  request was completed (i.e. not aborted). Note that for
            --  a Sync_With_Server request, the ORB normally generates an
            --  Acknowledge_Request after the servant has been located and
            --  prior to making the upcall, but it can also generate an
            --  Executed_Request to return an exception in the case where
            --  servant location failed (denoted by Surrogate = null).

            if not Req.Aborted
                 and then
               (Is_Set (Sync_With_Target, Req.Req_Flags)
                  or else
                Is_Set (Sync_Call_Back, Req.Req_Flags)
                  or else
                (Is_Set (Sync_With_Server, Req.Req_Flags)
                   and then Req.Surrogate = null))
            then
               Send_Reply (Session_Access (Sess), Req);
            end if;

            Destroy_Request (Req);
         end;

      elsif S in Acknowledge_Request then
         if Is_Set (Sync_With_Server,
                    Acknowledge_Request (S).Req.Req_Flags)
         then
            Send_Reply (Session_Access (Sess), Acknowledge_Request (S).Req);
         end if;

      else
         return Filters.Handle_Message (Filters.Filter (Sess.all)'Access, S);
      end if;

      return Nothing;
   end Handle_Message;

   -------------------
   -- Get_Task_Info --
   -------------------

   function Get_Task_Info
     (S : Session_Access)
     return PolyORB.Annotations.Notepad_Access is
   begin
      return S.N;
   end Get_Task_Info;

   -------------------
   -- Set_Task_Info --
   -------------------

   procedure Set_Task_Info
     (S : Session_Access;
      N : PolyORB.Annotations.Notepad_Access) is
   begin
      S.N := N;
   end Set_Task_Info;

end PolyORB.Protocols;
