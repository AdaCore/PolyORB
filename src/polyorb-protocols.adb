------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P R O T O C O L S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Support for object method invocation protocols.

--  $Id$

with Ada.Tags;

with PolyORB.Filters.Interface;
with PolyORB.If_Descriptors;
with PolyORB.Log;
with PolyORB.Objects.Interface;
with PolyORB.Protocols.Interface;
with PolyORB.Types;

package body PolyORB.Protocols is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Objects.Interface;
   use PolyORB.Protocols.Interface;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Session)
   is
      --  V : Version_Id;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);
      pragma Debug (O ("Finalizing Session."));
      --      if S.Request_Watcher /= null then
      --  Create (S.Finalize_Watcher);
      --  Lookup (S.Finalize_Watcher, V);
      --  S.Is_Open := False;
      --  Update (S.Request_Watcher);
      --  Differ (S.Finalize_Watcher, V);
      --  Destroy (S.Request_Watcher);
      --  Destroy (S.Finalize_Watcher);
      --  Deallocate (S.Request_List);
      --  end if;
      null;
   end Finalize;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (S    : access Session;
      Args : in out Any.NVList.Ref) is
   begin
      raise Program_Error;
      --  By default: no support for deferred arguments
      --  unmarshalling.
   end Handle_Unmarshall_Arguments;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Sess : access Session;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      Nothing : Components.Null_Message;
      Req : Request_Access;
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
              (Session_Access (Sess), Args);
            return Unmarshalled_Arguments'(Args => Args);
         end;
      elsif S in Set_Server then
         Sess.Server := Set_Server (S).Server;
      elsif S in Execute_Request then
         Req := Execute_Request (S).Req;

         if Req.Deferred_Arguments_Session /= null then

            --  This session object participates in a proxy
            --  construct: now is the last place we can determine
            --  the signature of the called method in order to
            --  translate the request.

            --  XXX this may require an interface repository lookup,
            --      which is not implemented. For now we do our best,
            --      hoping that the protocol on the server session
            --      can make sense of the args without an arg list.

            declare
               use Protocols.Interface;
               use PolyORB.If_Descriptors;

               Desc : If_Descriptor_Access
                 renames Default_If_Descriptor;

               Args : Any.NVList.Ref
                 := Get_Empty_Arg_List
                 (Desc, Req.Target,
                  Types.To_Standard_String (Req.Operation));

               Reply : constant Components.Message'Class
                 := Components.Emit
                 (Req.Deferred_Arguments_Session,
                  Unmarshall_Arguments'(Args => Args));

            begin
               pragma Assert (Reply in Unmarshalled_Arguments);
               pragma Debug (O ("Unmarshalled deferred arguments"));
               Req.Args := Unmarshalled_Arguments (Reply).Args;
               Req.Result.Argument := Get_Empty_Result
                 (Desc, Req.Target,
                  Types.To_Standard_String (Req.Operation));
               Req.Deferred_Arguments_Session := null;
               pragma Debug (O ("Proxying request: " & Image (Req.all)));
            end;

         end if;

         Invoke_Request
           (Session_Access (Sess), Req, Execute_Request (S).Pro);

         --  At this point, the request has been sent to the server
         --  'With_Transport' synchronisation policy has been completed.

         if Is_Set (Sync_With_Transport, Req.Req_Flags)
           or else Is_Set (Sync_Call_Back, Req.Req_Flags)
         then
            Req.Completed := True;
         end if;

      elsif S in Executed_Request then
         declare
            Req : Request_Access
              := Executed_Request (S).Req;
         begin
            --  Send reply only if expected.
            if Is_Set (Sync_With_Target, Req.Req_Flags) or
              Is_Set (Sync_Call_Back, Req.Req_Flags) then
               Send_Reply (Session_Access (Sess), Req);
               Destroy_Request (Req);

            elsif Is_Set (Sync_With_Server, Req.Req_Flags) then
               Send_Reply (Session_Access (Sess), Req);

               --   XXX The request has been deleted otherwise
            end if;
         end;

      elsif S in Disconnect_Request then
         return Emit (Lower (Sess), S);
      else
         raise Components.Unhandled_Message;
      end if;
      return Nothing;
   end Handle_Message;

   -------------------------
   -- Get_Task_Info --
   -------------------------

   function Get_Task_Info
     (S : in Session_Access)
     return PolyORB.Annotations.Notepad_Access
   is
   begin
      return S.N;
   end Get_Task_Info;

   -------------------------
   -- Set_Request_Watcher --
   -------------------------

   procedure Set_Task_Info
     (S : in Session_Access;
      N : PolyORB.Annotations.Notepad_Access)
   is
   begin
      S.N := N;
   end Set_Task_Info;

   --    -------------------------
   --    -- Get_First_Request --
   --    -------------------------

   --    procedure Get_First_Request
   --      (S      : in out Session_Access;
   --       Result : out Request_Info)
   --    is
   --    begin
   --       pragma Debug (O ("Get Length : "
   --                        & Integer'Image (Length (S.Request_List))));
   --       Request_Queue.Extract_Element (S.Request_List, 0, Result);
   --       pragma Debug (O ("Get Length : "
   --                        & Integer'Image (Length (S.Request_List))));
   --    end Get_First_Request;

   --    -----------------
   --    -- Add_Request --
   --    -----------------
   --    procedure Add_Request
   --      (S : in out Session_Access;
   --       RI : Request_Info)
   --    is
   --    begin
   --       pragma Debug (O ("Add Length : "
   --                        & Integer'Image (Length (S.Request_List))));
   --       Request_Queue.Append (S.Request_List, RI);
   --       pragma Debug (O ("Add Length : "
   --                        & Integer'Image (Length (S.Request_List))));
   --       Update (S.Request_Watcher);
   --    end Add_Request;

   -------------
   -- Is_Open --
   -------------
   --  function Is_Open
   --      (S : in Session_Access)
   --      return Boolean
   --    is
   --    begin
   --       return S.Is_Open;
   --    end Is_Open;

   -----------------------
   -- Can_Close_Session --
   -----------------------
   --    procedure Can_Close_Session
   --      (S : Session_Access)
   --    is
   --    begin
   --       Update (S.Finalize_Watcher);
   --    end Can_Close_Session;

end PolyORB.Protocols;
