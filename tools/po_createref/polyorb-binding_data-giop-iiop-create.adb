------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.BINDING_DATA.GIOP.IIOP.CREATE                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with PolyORB.Binding_Data.Create;
with PolyORB.Initialization;

with PolyORB.GIOP_P.Tagged_Components.Create;
with PolyORB.GIOP_P.Transport_Mechanisms;
with PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
with PolyORB.Sockets;
with PolyORB.POA_Types;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.GIOP.IIOP.Create is

   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Tagged_Components.Create;
   use PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.POA_Types;
   use PolyORB.Sockets;
   use PolyORB.Utils.Sockets;

   -------------------------
   -- Create_IIOP_Profile --
   -------------------------

   procedure Create_IIOP_Profile
     (Param          : Parameter_Profile;
      Profile        : out PolyORB.Binding_Data.Profile_Access;
      Error          : out Boolean)
   is
      use PolyORB.Utils;
   begin
      --  <Object_Id> -vmj <Major> -vmn <Minor>
      --   -a <IP_Address> -p <Port> -cn <Component_Number>
      --   {components} pe

      --  where object_id :=
      --  -i <Object_Index> [-g] -cr <Creator>

      Error := False;
      Profile := new IIOP_Profile_Type;

      declare
         UOid     : Unmarshalled_Oid;
         TProfile : IIOP_Profile_Type renames IIOP_Profile_Type (Profile.all);
      begin
         --  We build a IIOP profile

         UOid := Create_Id
           (Name             => Param.Index.all,
            System_Generated => Param.Is_Generated,
            Persistency_Flag => Time_Stamp'First,
            Creator          => Param.Creator_Name.all);
         TProfile.Object_Id := U_Oid_To_Oid (UOid);

         --  Set version
         TProfile.Version_Major := Param.Version_Major;
         TProfile.Version_Minor := Param.Version_Minor;

         --  Create server address and add transport mechanism

         Append
           (TProfile.Mechanisms,
            PolyORB.GIOP_P.Transport_Mechanisms.IIOP.Create_Transport_Mechanism
            (Param.Address.Inet_Addr.all + Port_Type (Param.Address.Port)));

         --  Add Tagged_Components

         if Param.Components /= null then
            Create_Tagged_Components
              (Param.Components.all, TProfile.Components, Error);
         end if;
      end;

   end Create_IIOP_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Binding_Data.Create.Register
        ("iiop", Create_IIOP_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.binding_data.iiop.create",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.GIOP.IIOP.Create;
