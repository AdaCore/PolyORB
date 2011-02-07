------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . N A M E _ S E R V I C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2011, Free Software Foundation, Inc.          --
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

with PolyORB.DSA_P.Name_Service.mDNS;
with PolyORB.DSA_P.Name_Service.COS_Naming;
with PolyORB.Parameters;
with PolyORB.Binding_Data;
with System.RPC;
with PolyORB.Log;
with PolyORB.Utils;
with PolyORB.Errors;
with PolyORB.Components;
with PolyORB.Setup;
with PolyORB.References.Binding;

package body PolyORB.DSA_P.Name_Service is
   use PolyORB.Log;
   use PolyORB.DSA_P.Name_Service.mDNS;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.dsa_p.name_service");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   procedure Initialize_Name_Server is
      use PolyORB.Binding_Data;
      use PolyORB.References;

      Name_Server_String : constant String :=
        PolyORB.Parameters.Get_Conf ("dsa", "name_context", "COS");
   begin
      pragma Debug (C, O ("Initialize_Name_Server : Enter"));

      --  If mDNS is configured by user

      if Name_Server_String = "MDNS" then
         Name_Ctx := new PolyORB.DSA_P.Name_Service.mDNS.MDNS_Name_Server;

         declare
            Nameservice_Location : constant String :=
              PolyORB.Parameters.Get_Conf ("dsa", "name_service");
         begin
            PolyORB.DSA_P.Name_Service.mDNS.Initiate_MDNS_Context
              (Nameservice_Location, Name_Ctx);
         end;

      --  COS Naming case

      else
         Name_Ctx := new PolyORB.DSA_P.Name_Service.COS_Naming.COS_Name_Server;
         declare
            Nameserver_Location : constant String :=
              PolyORB.Parameters.Get_Conf ("dsa", "name_service");
         begin
            PolyORB.References.String_To_Object
              (Nameserver_Location, Name_Ctx.Base_Ref);

         exception
            when others =>
               raise System.RPC.Communication_Error
             with "unable to locate name server " & Nameserver_Location;
         end;
      end if;

      Max_Requests :=
        PolyORB.Parameters.Get_Conf
          (Section => "dsa",
           Key     => "max_failed_requests",
           Default => 10);

      pragma Debug (C, O ("Initialize_Name_Server : Leave"));
   end Initialize_Name_Server;

   ---------------------
   -- Get_Name_Server --
   ---------------------

   function Get_Name_Server return Name_Server_Access is
   begin
      return Name_Ctx;
   end Get_Name_Server;

   -----------------------------
   -- Get_Reconnection_Policy --
   -----------------------------

   function Get_Reconnection_Policy
     (Name : String) return Reconnection_Policy_Type is
   begin
      return Reconnection_Policy_Type'Value
               (PolyORB.Parameters.Get_Conf
                  (Section => "dsa",
                   Key     => RCI_Attr (Name, Reconnection),
                   Default => Default_Reconnection_Policy'Img));
   end Get_Reconnection_Policy;

   --------------
   -- RCI_Attr --
   --------------

   function RCI_Attr (Name : String; Attr : RCI_Attribute) return String
   is
      use PolyORB.Utils;
   begin
      return To_Lower (Name & "'" & Attr'Img);
   end RCI_Attr;

   ------------------------
   -- Is_Reference_Valid --
   ------------------------

   function Is_Reference_Valid (R : PolyORB.References.Ref) return Boolean
   is
      use PolyORB.References.Binding;
      use PolyORB.Errors;

      S            : PolyORB.Components.Component_Access;
      Pro          : PolyORB.Binding_Data.Profile_Access;
      Error        : PolyORB.Errors.Error_Container;
   begin

      --  Bind the reference to ensure validity

      Bind (R          => R,
            Local_ORB  => PolyORB.Setup.The_ORB,
            Servant    => S,
            QoS        => (others => null),
            Pro        => Pro,
            Local_Only => False,
            Error      => Error);

      if Found (Error) then
         Catch (Error);
         return False;
      end if;
      return True;
   exception
      when others =>
         return False;
   end Is_Reference_Valid;

end PolyORB.DSA_P.Name_Service;
