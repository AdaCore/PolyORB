------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . N A M E _ S E R V I C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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
with System.RPC;

package body PolyORB.DSA_P.Name_Service is

   procedure Initialize_Name_Context
     (Name_Ctx : in out Name_Context_Access)
   is
      Name_Context_String : constant String :=
                                    PolyORB.Parameters.Get_Conf
                                      ("dsa", "name_context", "COS");
   begin

      --  If mDNS is configured by user
      if Name_Context_String = "MDNS" then
         Name_Ctx := new PolyORB.DSA_P.Name_Service.mDNS.MDNS_Name_Context;
         declare
            Nameservice_Location : constant String :=
                                    PolyORB.Parameters.Get_Conf
                                      ("dsa", "name_service");
         begin
            PolyORB.DSA_P.Name_Service.mDNS.Initiate_MDNS_Context
              (Nameservice_Location, Name_Ctx);
         end;
      --  COS Naming case
      else
         Name_Ctx
           := new PolyORB.DSA_P.Name_Service.COS_Naming.COS_Name_Context;
         declare
            Nameserver_Location : constant String :=
                                    PolyORB.Parameters.Get_Conf
                                      ("dsa", "name_service");
         begin
            PolyORB.References.String_To_Object
              (Nameserver_Location, Name_Ctx.Base_Ref);
         exception
            when others =>
               raise System.RPC.Communication_Error
             with "unable to locate name server " & Nameserver_Location;
         end;
      end if;

   end Initialize_Name_Context;

end PolyORB.DSA_P.Name_Service;
