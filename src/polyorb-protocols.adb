------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P R O T O C O L S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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
   pragma Unreferenced (C); --  For conditional pragma Debug

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

   function Handle_Message
     (Sess : access Session;
      S    :        Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Errors;

      Nothing : Components.Null_Message;
      Req     : Request_Access;
      Error   : Errors.Error_Container;

   begin
      pragma Debug
        (O ("Handling message of type "
            & Ada.Tags.External_Tag (S'Tag)));
      if S in Connect_Indication then
         Handle_Connect_Indication (Session_Access (Sess));

      elsif S in Connect_Confirmation then
         Handle_Connect_Confirmation (Session_Access (Sess));

      elsif S in Disconnect_Indication then
         Handle_Disconnect (Session_Access (Sess));

      elsif S in Data_Indication then
         Handle_Data_Indication
           (Session_Access (Sess),
            Data_Indication (S).Data_Amount);

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
         Sess.Server := Set_Server (S).Server;
         Sess.Dependent_Binding_Object
           := Set_Server (S).Binding_Object;

      elsif S in Execute_Request then
         declare
            use type Binding_Data.Profile_Access;
         begin
            pragma Assert (Execute_Request (S).Pro /= null);
            null;
         end;

         Req := Execute_Request (S).Req;

         if Req.Deferred_Arguments_Session /= null then

            --  This session object participates in a proxy
            --  construct: now is the last place we can determine
            --  the signature of the called method in order to
            --  translate the request. As we do not possess the
            --  actual servant on the local node, we need another
            --  way of retrieving an interface description (i.e.
            --  a parameter and result profile). This is typically
            --  achieved by looking up the target interface in an
            --  interface repository. In PolyORB, such operations
            --  are abstracted by the If_Descriptor interface.

            declare
               use PolyORB.If_Descriptors;

               Desc : If_Descriptor_Access renames Default_If_Descriptor;
               --  Delegate the decision and lookup process to
               --  the default interface descriptor objet.

               Args : Any.NVList.Ref
                 := Get_Empty_Arg_List (Desc, Req.Target, Req.Operation.all);

               Reply : constant Components.Message'Class
                 := Components.Emit
                 (Req.Deferred_Arguments_Session,
                  Unmarshall_Arguments'(Args => Args));

            begin
               pragma Assert (Reply in Unmarshalled_Arguments
                                or else Reply in Arguments_Error);
               if Reply in Unmarshalled_Arguments then
                  pragma Debug (O ("Unmarshalled deferred arguments"));
                  Req.Args := Unmarshalled_Arguments (Reply).Args;
                  Req.Result.Argument := Get_Empty_Result
                    (Desc, Req.Target, Req.Operation.all);

                  Req.Deferred_Arguments_Session := null;
                  pragma Debug (O ("Proxying request: " & Image (Req.all)));

               else
                  pragma Debug (O ("Unmarshall deferred arguments error"));
                  Set_Exception (Req, Arguments_Error (Reply).Error);

                  --  Free data associated to Arguments_Error (Reply).Error

                  declare
                     Error : Error_Container := Arguments_Error (Reply).Error;
                  begin
                     Catch (Error);
                  end;
               end if;
            end;
         end if;

         if not Found (Error) then
            Invoke_Request
              (Session_Access (Sess), Req, Execute_Request (S).Pro);
         end if;

         --  At this point, the request has been sent to the server
         --  'With_Transport' synchronisation policy has been completed.

         if Is_Set (Sync_With_Transport, Req.Req_Flags)
           or else Is_Set (Sync_Call_Back, Req.Req_Flags)
         then
            Req.Completed := True;
         end if;

         if Found (Error) then
            return Executed_Request'(Req => Req);
         end if;

      elsif S in Executed_Request then
         declare
            Req : Request_Access
              := Executed_Request (S).Req;
         begin

            if Req.Deferred_Arguments_Session /= null then

               --  The request has been aborted before being fully
               --  processed. Flush the session's data and restore the
               --  session to its initial state, waiting for requests.

               Emit_No_Reply
                 (Component_Access (Sess),
                  Protocols.Iface.Flush'(Message with null record));
            end if;

            if Is_Set (Sync_With_Target, Req.Req_Flags)
              or else Is_Set (Sync_Call_Back, Req.Req_Flags)
            then
               --  Send a reply if one is expected.

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

      elsif S in Disconnect_Request then
         return Emit (Lower (Sess), S);

      else
         raise Program_Error;
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
