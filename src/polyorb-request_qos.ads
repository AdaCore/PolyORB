------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . R E Q U E S T _ Q O S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the Quality of Service (QoS) parameters to be
--  passed along with a PolyORB request, and the call-back functions
--  used to retrieve them.

--  $Id$

with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Tasking.Priorities;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Request_QoS is

   package PR  renames PolyORB.Requests;
   package PTP renames PolyORB.Tasking.Priorities;

   --  List of supported QoS policies

   type QoS_Kind is
     (None,
      Static_Priority);

   --  Definition of QoS parameters

   type QoS_Parameter (Kind : QoS_Kind) is record
      case Kind is
         when None =>
            null;

         when Static_Priority =>
            OP : PTP.ORB_Priority;
            EP : PTP.External_Priority;
            --  XXX these components need to have explicit names
            --  and documentation.
      end case;

   end record;

   type QoS_Parameter_Access is access all QoS_Parameter;

   package QoS_Parameter_Lists is
      new PolyORB.Utils.Chained_Lists (QoS_Parameter_Access);

   subtype QoS_Parameters is QoS_Parameter_Lists.List;
   --  XXX should rethink this container, what about a static array ?

   function Fetch_QoS
     (Ref : PolyORB.References.Ref)
     return QoS_Parameter_Lists.List;
   --  Return the list of the QoS parameters to be applied when
   --  sending a request to the target denoted by Ref. This functions
   --  iterated over the different call-backs.

   function Extract_Request_Parameter
     (Kind : QoS_Kind;
      Req  : PR.Request_Access)
     return QoS_Parameter;
   --  Return QoS parameter of type Kind from QoS, or a QoS_Parameter
   --  of kind None if no parameter matches Kind.

   procedure Set_Request_QoS (Req : PR.Request_Access; QoS : QoS_Parameters);

   function Get_Request_QoS (Req : PR.Request_Access) return QoS_Parameters;

   procedure Add_Reply_QoS (Req : PR.Request_Access; QoS : QoS_Parameter);
   --  Add (replace if exists) passed QoS to the list of reply QoSs.

   function Get_Reply_QoS (Req : PR.Request_Access) return QoS_Parameters;

   type Fetch_QoS_CB is access function
     (Ref : PolyORB.References.Ref)
     return QoS_Parameter_Access;
   --  This call-back function retrieves one QoS_Parameter to be
   --  applied when sending a request to the target denoted by Ref.
   --  Return null if QoS parameter is not applicable for Ref.

   procedure Register (Kind : QoS_Kind; CB : Fetch_QoS_CB);
   --  Register one call-back function attached to QoS_Kind Kind

   function Image (QoS : QoS_Parameter_Lists.List) return String;
   --  For debugging purposes. Return an image of QoS

end PolyORB.Request_QoS;
