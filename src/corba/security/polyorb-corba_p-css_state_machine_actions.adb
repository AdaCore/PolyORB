------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.CORBA_P.CSS_STATE_MACHINE_ACTIONS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Streams;

with PolyORB.Annotations;
with PolyORB.CORBA_P.Security_Current;
with PolyORB.Security.Authentication_Mechanisms;
with PolyORB.Security.Authority_Mechanisms;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Tasking.Threads.Annotations;

package body PolyORB.CORBA_P.CSS_State_Machine_Actions is

   use PolyORB.Security.Authentication_Mechanisms;
   use PolyORB.Security.Types;

   ----------------------
   -- Complete_Context --
   ----------------------

   procedure Complete_Context
     (Connection          : PolyORB.Security.Connections.Connection_Access;
      Client_Context_Id   : PolyORB.Security.Types.Context_Id;
      Context_Stateful    : Boolean;
      Final_Context_Token :
        PolyORB.Security.Types.Stream_Element_Array_Access)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (Client_Context_Id);
      pragma Unreferenced (Context_Stateful);
      pragma Unreferenced (Final_Context_Token);

   begin
      null;
   end Complete_Context;

   ----------------------------
   -- Get_Client_Credentials --
   ----------------------------

   function Get_Client_Credentials
     (Policy    : PolyORB.CORBA_P.Security_Policy.Client_Policy;
      Mechanism : PolyORB.QoS.Clients_Security.Client_Mechanism_Access)
      return PolyORB.Security.Credentials.Credentials_Ref
   is
      use PolyORB.Security.Credentials;
      use PolyORB.Security.Transport_Mechanisms;

      Credentials_Supports : Association_Options;
      Failed               : Boolean;
      Result               : Credentials_Ref;

   begin
      --  At this point we should decide to use only own credentials
      --  or create special credentials which contained selected own
      --  credentials (for transport layer and authentication layer) and
      --  received credentials (for creation of identity assertion token and
      --  authorization token at attribute layer).

      --  Find appropriate credentials in Security Manager own credentials

      --  XXX CORBA Security Service: we also need to analize Invocation
      --  Credentials Policy. In proper order, of course.

      for J in Policy.Invocation_Credentials'Range loop
         Credentials_Supports := 0;
         Failed               := False;

         --  Is current credentials satisfy Transport Mechanism level
         --  requirements?

         if Mechanism.Transport /= null
           and then Target_Supports (Mechanism.Transport) /= 0
         then
            --  Transport layer is security aware

            if Is_Supports
              (Mechanism.Transport, Policy.Invocation_Credentials (J))
            then
               Credentials_Supports :=
                 Credentials_Supports
                 or Get_Invocation_Options_Supported
                 (Credentials_Access
                  (Entity_Of (Policy.Invocation_Credentials (J))));

            else
               Failed := True;
            end if;
         end if;

         --  Is current credentials satisfy Authentication Mechanism level
         --  requirements?

         if Mechanism.Authentication_Mechanism /= null then
            --  Authentication layer present

            if Is_Supports
              (Mechanism.Authentication_Mechanism,
               Policy.Invocation_Credentials (J))
            then
               Credentials_Supports :=
                 Credentials_Supports
                 or Get_Invocation_Options_Supported
                 (Credentials_Access
                  (Entity_Of (Policy.Invocation_Credentials (J))));

            elsif Mechanism.Authentication_Required then
               --  Authentication layer required but not suppoted

               Failed := True;
            end if;
         end if;

         --  XXX Delegation???

         if not Failed
           and then Is_Set (Policy.Client_Requires, Credentials_Supports)
         then
            Result := Policy.Invocation_Credentials (J);
            exit;
         end if;
      end loop;

      return Result;
   end Get_Client_Credentials;

   -------------------------
   -- Get_Context_Element --
   -------------------------

   function Get_Context_Element
     (Policy      : PolyORB.CORBA_P.Security_Policy.Client_Policy;
      Mechanism   : PolyORB.QoS.Clients_Security.Client_Mechanism_Access;
      Credentials : PolyORB.Security.Credentials.Credentials_Ref;
      Connection  : PolyORB.Security.Connections.Connection_Access)
      return Context_Element
   is
      pragma Unreferenced (Connection);

      use PolyORB.CORBA_P.Security_Current;
      use PolyORB.Security.Authority_Mechanisms;
      use
        PolyORB.Security.Authority_Mechanisms.Client_Authority_Mechanism_Lists;
      use PolyORB.Security.Authorization_Elements;
      use PolyORB.Security.Identities;

      Authentication_Token : Stream_Element_Array_Access;
      Identity_Token       : Identity_Access;
      Authorization_Token  : Authorization_Element_Lists.List;
      Current_Note         : Security_Current_Note;
      Success              : Boolean;

   begin
      PolyORB.Annotations.Get_Note
        (PolyORB.Tasking.Threads.Annotations.Get_Current_Thread_Notepad.all,
         Current_Note,
         Empty_Security_Current_Note);

      if Mechanism.Authentication_Mechanism /= null then
         if Mechanism.Authentication_Required
           or else Is_Set
           (Establish_Trust_In_Client,
            Policy.Client_Requires)
         then
            Authentication_Token :=
              new Ada.Streams.Stream_Element_Array'
              (PolyORB.Security.Authentication_Mechanisms.Init_Security_Context
               (Mechanism.Authentication_Mechanism,
                Credentials));
         end if;
      end if;

      --  Retrieve authorization token from the privilege authority

      if Mechanism.Authorities /= Client_Authority_Mechanism_Lists.Empty then
         Get_Authorization_Token
           (Element (Mechanism.Authorities, 0).all,
            Identity_Token,
            Authorization_Token,
            Success);

         --  XXX Error processing should be investigated.
      end if;

      if Mechanism.Identity_Assertion
        and then Current_Note.Access_Identity /= null
        and then Identity_Token = null
      then
         Identity_Token := Duplicate (Current_Note.Access_Identity);
      end if;

      if Authentication_Token = null
        and then Identity_Token = null
      then
         return Context_Element'(Kind => No_Element);

      else
         return Context_Element'
           (Kind                        => Establish_Context,
            Client_Context_Id           => 0,
            Client_Authentication_Token => Authentication_Token,
            Identity_Token              => Identity_Token,
            Authorization_Token         => Authorization_Token);
      end if;
   end Get_Context_Element;

   -------------------
   -- Get_Mechanism --
   -------------------

   procedure Get_Mechanism
     (Policy        : PolyORB.CORBA_P.Security_Policy.Client_Policy;
      Configuration :
        PolyORB.QoS.Clients_Security.Client_Mechanism_Lists.List;
      Mechanism     : out PolyORB.QoS.Clients_Security.Client_Mechanism_Access;
      Success       : out Boolean)
   is
      use PolyORB.QoS.Clients_Security;
      use PolyORB.QoS.Clients_Security.Client_Mechanism_Lists;

      Iter : Iterator := First (Configuration);

   begin
      Mechanism := null;
      Success   := False;

      --  If target supports only unprotected invocations and client don't
      --  requires any protection then process unprotected invocation

      if Last (Iter)
        and then Policy.Client_Requires = 0
      then
         Success := True;
      end if;

      --  Otherwise try to find mechanism which satisfy client's requirements

      while not Last (Iter) loop
         if Is_Set
           (Policy.Client_Requires, Target_Supports (Value (Iter).all.all))
         then
            Mechanism := Value (Iter).all;
            Success   := True;

            return;
         end if;

         Next (Iter);
      end loop;
   end Get_Mechanism;

   ------------------------
   -- Invalidate_Context --
   ------------------------

   procedure Invalidate_Context
     (Connection        : PolyORB.Security.Connections.Connection_Access;
      Client_Context_Id : PolyORB.Security.Types.Context_Id)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (Client_Context_Id);

   begin
      null;
   end Invalidate_Context;

   procedure Invalidate_Context
     (Connection        : PolyORB.Security.Connections.Connection_Access;
      Client_Context_Id : PolyORB.Security.Types.Context_Id;
      Error_Token       : PolyORB.Security.Types.Stream_Element_Array_Access)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (Client_Context_Id);
      pragma Unreferenced (Error_Token);

   begin
      null;
   end Invalidate_Context;

end PolyORB.CORBA_P.CSS_State_Machine_Actions;
