------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . S E C U R I T Y _ C O N T E X T S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

--  Request Security Service Context

with PolyORB.Security.Authorization_Elements;
with PolyORB.Security.Identities;
with PolyORB.Security.Types;
with PolyORB.Types;

package PolyORB.QoS.Security_Contexts is

   package PS renames PolyORB.Security;

   type Context_Kinds is
     (Establish_Context,
      Complete_Establish_Context,
      Context_Error,
      Message_In_Context);

   type QoS_Security_Context_Parameter (Context_Kind : Context_Kinds) is
     new QoS_Parameter (Compound_Security) with
   record
--  XXX This portion should be moved into separate QoS parameter!
--  It violates QoS <=> GIOP Service Context conversion rules!
--
--      Selected              :
--        PolyORB.Security.Compound_Mechanisms.Compound_Mechanism_Access;
--      --  Selected compound security mechanism
--
--      Transport_Credentials : PolyORB.Security.Credentials.Credentials_Ref;
--      --  Transport layer credentials. For client this credentials used for
--      --  establish connection with server (own credentials). For server this
--      --  is credentials of client, which establish connection (received
--      --  credentials).
--
--  XXX End of section

      Client_Context_Id : PolyORB.Security.Types.Context_Id;

      case Context_Kind is
         when Establish_Context =>
            Authorization_Token         :
              PS.Authorization_Elements.Authorization_Element_Lists.List;
            Identity_Token              :
              PolyORB.Security.Identities.Identity_Access;
            Client_Authentication_Token :
              PolyORB.Security.Types.Stream_Element_Array_Access;

         when Complete_Establish_Context =>
            Context_Stateful    : Boolean;
            Final_Context_Token :
              PolyORB.Security.Types.Stream_Element_Array_Access;

         when Context_Error =>
            Major_Status : PolyORB.Types.Long;
            Minor_Status : PolyORB.Types.Long;
            Error_Token  :
              PolyORB.Security.Types.Stream_Element_Array_Access;

         when Message_In_Context =>
            Discard_Context : Boolean;
      end case;
   end record;

   type QoS_Security_Context_Parameter_Access is
     access all QoS_Security_Context_Parameter;

   procedure Release_Contents
     (QoS : access QoS_Security_Context_Parameter);

end PolyORB.QoS.Security_Contexts;
