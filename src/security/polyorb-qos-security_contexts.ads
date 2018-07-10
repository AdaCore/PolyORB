------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . S E C U R I T Y _ C O N T E X T S         --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2012;

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

   overriding procedure Release_Contents
     (QoS : access QoS_Security_Context_Parameter);

end PolyORB.QoS.Security_Contexts;
