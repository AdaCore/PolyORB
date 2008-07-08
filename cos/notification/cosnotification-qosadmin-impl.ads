------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        C O S N O T I F I C A T I O N . Q O S A D M I N . I M P L         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  with CORBA;

with PortableServer;

package CosNotification.QoSAdmin.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_QoS
     (Self : access Object)
     return CosNotification.QoSProperties;

   procedure Set_QoS
     (Self : access Object;
      QoS  : in CosNotification.QoSProperties);

   procedure Validate_QoS
     (Self          : access Object;
      Required_QoS  : in CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

private

   type QoS_Admin_Record;
   type QoS_Admin_Access is access QoS_Admin_Record;

   type Object is new PortableServer.Servant_Base with record
      X : QoS_Admin_Access;
   end record;

end CosNotification.QoSAdmin.Impl;
