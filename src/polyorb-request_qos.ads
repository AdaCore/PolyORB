------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . R E Q U E S T _ Q O S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  This package defines the Quality of Service (QoS) parameters to be
--  passed along with a PolyORB request, and the call-back functions
--  used to retrieve them.

with PolyORB.QoS;
with PolyORB.References;
with PolyORB.Requests;

package PolyORB.Request_QoS is

   function Fetch_QoS
     (Ref : PolyORB.References.Ref)
      return PolyORB.QoS.QoS_Parameters;
   --  Return the list of the QoS parameters to be applied when
   --  sending a request to the target denoted by Ref. This functions
   --  iterated over the different call-backs.

   procedure Set_Request_QoS
     (Req : PolyORB.Requests.Request_Access;
      QoS : PolyORB.QoS.QoS_Parameters);

   procedure Add_Request_QoS
     (Req  : PolyORB.Requests.Request_Access;
      Kind : PolyORB.QoS.QoS_Kind;
      QoS  : PolyORB.QoS.QoS_Parameter_Access);
   --  Add (replace if exists) passed QoS to the list of request QoSs.

   procedure Add_Reply_QoS
     (Req  : PolyORB.Requests.Request_Access;
      Kind : PolyORB.QoS.QoS_Kind;
      QoS  : PolyORB.QoS.QoS_Parameter_Access);
   --  Add (replace if exists) passed QoS to the list of reply QoSs.

   function Extract_Request_Parameter
     (Kind : PolyORB.QoS.QoS_Kind;
      Req  : PolyORB.Requests.Request_Access)
      return PolyORB.QoS.QoS_Parameter_Access;
   --  Return QoS parameter of type Kind from request QoS, or a null
   --  if no parameter matches Kind.

   function Extract_Reply_Parameter
     (Kind : PolyORB.QoS.QoS_Kind;
      Req  : PolyORB.Requests.Request_Access)
      return PolyORB.QoS.QoS_Parameter_Access;
   --  Return QoS parameter of type Kind from reply QoS, or a null
   --  if no parameter matches Kind.

   type Fetch_QoS_CB is access function
     (Ref : PolyORB.References.Ref)
      return PolyORB.QoS.QoS_Parameter_Access;
   --  This call-back function retrieves one QoS_Parameter to be
   --  applied when sending a request to the target denoted by Ref.
   --  Return null if QoS parameter is not applicable for Ref.

   procedure Register (Kind : PolyORB.QoS.QoS_Kind; CB : Fetch_QoS_CB);
   --  Register one call-back function attached to QoS_Kind Kind

   function Get_Request_QoS
     (Req : PolyORB.Requests.Request_Access)
      return PolyORB.QoS.QoS_Parameters;

   function Get_Reply_QoS
     (Req : PolyORB.Requests.Request_Access)
      return PolyORB.QoS.QoS_Parameters;

   procedure Set_Reply_QoS
     (Req : PolyORB.Requests.Request_Access;
      QoS : PolyORB.QoS.QoS_Parameters);

end PolyORB.Request_QoS;
