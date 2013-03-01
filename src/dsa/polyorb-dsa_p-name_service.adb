------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . N A M E _ S E R V I C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2013, Free Software Foundation, Inc.          --
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

with System.RPC;

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.References.Binding;
with PolyORB.Setup;
with PolyORB.Utils;

package body PolyORB.DSA_P.Name_Service is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.name_service");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ----------------------------
   -- Initialize_Name_Server --
   ----------------------------

   procedure Initialize_Name_Server is
      use PolyORB.Binding_Data;
      use PolyORB.References;

      Nameservice_Kind : constant String :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "name_server_kind",
           Default => "");

      Nameservice_Location : constant String :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "name_service");

   begin
      pragma Debug (C, O ("Initialize_Name_Server: enter"));

      Name_Ctx.Initialize
        (Parameters.Get_Conf
           (Section => "dsa",
            Key     => "name_service"));

      Time_Between_Requests :=
        PolyORB.Parameters.Get_Conf
          (Section => "dsa",
           Key     => "delay_between_failed_requests",
           Default => 1.0);

      Max_Requests :=
        PolyORB.Parameters.Get_Conf
          (Section => "dsa",
           Key     => "max_failed_requests",
           Default => 10);

      pragma Debug (C, O ("Initialize_Name_Server: leave"));
   exception
      when others =>
         raise System.RPC.Communication_Error with
           "unable to locate name service "
           & Nameservice_Location & " (" & Nameservice_Kind & ")";
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
