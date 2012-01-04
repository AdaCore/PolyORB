------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . R E Q U E S T _ Q O S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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
     (Req : in out Requests.Request;
      QoS : PolyORB.QoS.QoS_Parameters);

   procedure Add_Request_QoS
     (Req  : in out Requests.Request;
      Kind : PolyORB.QoS.QoS_Kind;
      QoS  : PolyORB.QoS.QoS_Parameter_Access);
   --  Add (replace if exists) passed QoS to the list of request QoSs.

   procedure Add_Reply_QoS
     (Req  : in out Requests.Request;
      Kind : PolyORB.QoS.QoS_Kind;
      QoS  : PolyORB.QoS.QoS_Parameter_Access);
   --  Add (replace if exists) passed QoS to the list of reply QoSs.

   function Extract_Request_Parameter
     (Kind : PolyORB.QoS.QoS_Kind;
      Req  : Requests.Request) return PolyORB.QoS.QoS_Parameter_Access;
   --  Return QoS parameter of type Kind from request QoS, or a null
   --  if no parameter matches Kind.

   function Extract_Reply_Parameter
     (Kind : PolyORB.QoS.QoS_Kind;
      Req  : Requests.Request) return PolyORB.QoS.QoS_Parameter_Access;
   --  Return QoS parameter of type Kind from reply QoS, or a null
   --  if no parameter matches Kind.

   type Fetch_QoS_CB is access function
     (Ref : PolyORB.References.Ref) return PolyORB.QoS.QoS_Parameter_Access;
   --  This call-back function retrieves one QoS_Parameter to be applied when
   --  sending a request to the target denoted by Ref. Return null if QoS
   --  parameter is not applicable for Ref.

   procedure Register (Kind : PolyORB.QoS.QoS_Kind; CB : Fetch_QoS_CB);
   --  Register one call-back function attached to QoS_Kind Kind

   function Get_Request_QoS
     (Req : Requests.Request) return PolyORB.QoS.QoS_Parameters;

   function Get_Reply_QoS
     (Req : Requests.Request) return PolyORB.QoS.QoS_Parameters;

   procedure Set_Reply_QoS
     (Req : in out Requests.Request;
      QoS : PolyORB.QoS.QoS_Parameters);

end PolyORB.Request_QoS;
