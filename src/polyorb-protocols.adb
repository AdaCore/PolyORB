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
with Ada.Unchecked_Deallocation;

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects.Interface;
with PolyORB.Protocols.Interface;

package body PolyORB.Protocols is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Objects.Interface;
   use PolyORB.ORB.Interface;
   use PolyORB.Protocols.Interface;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new Ada.Unchecked_Deallocation
     (Session'Class, Session_Access);

   procedure Destroy_Session (S : in out Session_Access) is
   begin
      Free (S);
   end Destroy_Session;

   procedure Handle_Unmarshall_Arguments
     (S    : access Session;
      Args : in out Any.NVList.Ref) is
   begin
      raise Program_Error;
      --  By default: no support for deferred arguments
      --  unmarshalling.
   end Handle_Unmarshall_Arguments;

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

               Args : Any.NVList.Ref;
               Reply : constant Components.Message'Class
                 := Components.Emit
                 (Req.Deferred_Arguments_Session,
                  Unmarshall_Arguments'(Args => Args));
            begin
               pragma Assert (Reply in Unmarshalled_Arguments);
               pragma Debug (O ("Unmarshalled deferred arguments"));
               Args := Unmarshalled_Arguments (Reply).Args;
               Req.Args := Args;
               Req.Deferred_Arguments_Session := null;
               pragma Debug (O ("Proxying request: " & Image (Req.all)));
            end;

         end if;

         Invoke_Request
           (Session_Access (Sess), Req, Execute_Request (S).Pro);

      elsif S in Executed_Request then
         declare
            Var_Req : Request_Access
              := Executed_Request (S).Req;
         begin
            Send_Reply
              (Session_Access (Sess),
               Executed_Request (S).Req);
            pragma Debug (O ("Destroying request..."));
            Destroy_Request (Var_Req);
            pragma Debug (O ("... done."));
         end;
      elsif S in Disconnect_Request then
         return Emit (Lower (Sess), S);
      else
         raise Components.Unhandled_Message;
      end if;
      return Nothing;
   end Handle_Message;

   -------------------------
   -- Get_Request_Watcher --
   -------------------------

   function Get_Request_Watcher
     (S : in Session_Access)
     return PolyORB.Soft_Links.Watcher_Access
   is
   begin
      return S.Request_Watcher;
   end Get_Request_Watcher;

   -------------------------
   -- Set_Request_Watcher --
   -------------------------

   procedure Set_Request_Watcher
     (S : in Session_Access;
      W : PolyORB.Soft_Links.Watcher_Access)
   is
   begin
      S.Request_Watcher := W;
   end Set_Request_Watcher;

   -------------------------
   -- Get_Pending_Request --
   -------------------------

   function Get_Pending_Request
     (S : in Session_Access)
     return ORB.Interface.Queue_Request
   is
   begin
      return S.Pending_Request;
   end Get_Pending_Request;

end PolyORB.Protocols;
