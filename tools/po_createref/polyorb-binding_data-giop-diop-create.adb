------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.BINDING_DATA.GIOP.DIOP.CREATE                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.Create;
with PolyORB.Initialization;

with PolyORB.GIOP_P.Tagged_Components.Create;
with PolyORB.GIOP_P.Transport_Mechanisms;
with PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
with PolyORB.POA_Types;
with PolyORB.Sockets;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.GIOP.DIOP.Create is

   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Tagged_Components.Create;
   use PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.POA_Types;
   use PolyORB.Sockets;
   use PolyORB.Utils.Sockets;

   -------------------------
   -- Create_DIOP_Profile --
   -------------------------

   procedure Create_DIOP_Profile
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
      Profile := new DIOP_Profile_Type;

      declare
         UOid     : Unmarshalled_Oid;
         TProfile : DIOP_Profile_Type renames DIOP_Profile_Type (Profile.all);
      begin
         --  We build a DIOP profile

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
            GIOP_P.Transport_Mechanisms.DIOP.Create_Transport_Mechanism
              (Param.Address.Inet_Addr.all + Port_Type (Param.Address.Port)));

         --  Add Tagged_Components

         Create_Tagged_Components
           (Param.Components.all, TProfile.Components, Error);
      end;

   end Create_DIOP_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Binding_Data.Create.Register
        ("diop", Create_DIOP_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.binding_data.diop.create",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.GIOP.DIOP.Create;
