------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
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

--  $Id$

with Ada.Tags;

with CORBA;

with PolyORB.Any;
with PolyORB.CORBA_P.Names;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Requests;
with PolyORB.Objects.Interface;
with PolyORB.Soft_Links;
with PolyORB.Utils.Chained_Lists;
pragma Elaborate_All (PolyORB.Utils.Chained_Lists);

package body PortableServer is

   use PolyORB.Log;
   use PolyORB.Soft_Links;

   package L is new PolyORB.Log.Facility_Log ("portableserver");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function Handle_Message
     (Self : access DynamicImplementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Objects.Interface;

   begin
      if Msg in Execute_Request then
         declare
            use PolyORB.Any;
            use PolyORB.Requests;
            use CORBA.ServerRequest;

            R : constant Request_Access
              := Execute_Request (Msg).Req;
         begin
            Invoke (DynamicImplementation'Class (Self.all)'Access,
                    CORBA.ServerRequest.Object_Ptr (R));
            --  Redispatch

            Pump_Up_Arguments
              (Dst_Args => R.Args, Src_Args => R.Out_Args,
               Direction => ARG_OUT,
               Ignore_Src_Mode => False);
            --  Copy back inout and out arguments from Out_Args
            --  to Args, so the requestor finds them where
            --  it expects.

            --  XXX If a method has IN and OUT args and R.Args
            --  contains only the IN arguments (and no empty
            --  Any's for the OUT ones) what happens?

            return Executed_Request'(Req => R);
         end;
      else
         raise PolyORB.Components.Unhandled_Message;
      end if;
   end Handle_Message;

   procedure Invoke
     (Self    : access Servant_Base;
      Request : in CORBA.ServerRequest.Object_Ptr) is
   begin
      Find_Info (Servant (Self)).Dispatcher (Servant (Self), Request);
      --  Invoke primitive for static object implementations:
      --  look up the skeleton associated with Self's class,
      --  and delegate the dispatching of Request to one of
      --  Self's primitive operations to that skeleton.
   end Invoke;

   function Get_Default_POA
     (For_Servant : Servant_Base)
     return POA_Forward.Ref is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Default_POA (For_Servant);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Get_Default_POA;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members) is
   begin
      raise PolyORB.Not_Implemented;
   end Get_Members;

   -----------------------------
   -- A list of Skeleton_Info --
   -----------------------------

   package Skeleton_Lists is new PolyORB.Utils.Chained_Lists
     (Skeleton_Info);

   All_Skeletons : Skeleton_Lists.List;

   Skeleton_Unknown : exception;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Info
     (For_Servant : Servant)
     return Skeleton_Info
   is
      use Skeleton_Lists;

      It : Iterator;
      Info    : Skeleton_Info;

   begin
      pragma Debug
        (O ("Find_Info: servant of type "
            & Ada.Tags.External_Tag (For_Servant'Tag)));
      Enter_Critical_Section;
      It := First (All_Skeletons);

      while not Last (It) loop
         pragma Debug (O ("... skeleton id: "
           & CORBA.To_Standard_String (Value (It).Type_Id)));
         exit when Value (It).Is_A (For_Servant);
         Next (It);
      end loop;

      if Last (It) then
         Leave_Critical_Section;
         raise Skeleton_Unknown;
      end if;

      Info := Value (It).all;
      Leave_Critical_Section;

      return Info;
   end Find_Info;

   -----------------------
   -- Register_Skeleton --
   -----------------------

   procedure Register_Skeleton
     (Type_Id    : in CORBA.RepositoryId;
      Is_A       : in Servant_Class_Predicate;
      Dispatcher : in Request_Dispatcher := null)
   is
      use Skeleton_Lists;
   begin
      pragma Debug (O ("Register_Skeleton enter"));
      Enter_Critical_Section;
      Prepend (All_Skeletons,
               (Type_Id    => Type_Id,
                Is_A       => Is_A,
                Dispatcher => Dispatcher));
      pragma Debug (O ("Registered : type_id = " &
                       CORBA.To_Standard_String (Type_Id)));
      Leave_Critical_Section;
   end Register_Skeleton;

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id
     (For_Servant : Servant)
     return CORBA.RepositoryId is
   begin
      return Find_Info (For_Servant).Type_Id;
   exception
      when Skeleton_Unknown =>
         return CORBA.To_CORBA_String
           (PolyORB.CORBA_P.Names.OMG_RepositoryId ("CORBA/OBJECT"));
      when others =>
         raise;
   end Get_Type_Id;

end PortableServer;
