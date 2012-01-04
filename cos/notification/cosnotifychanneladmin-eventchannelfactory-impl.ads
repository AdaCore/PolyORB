------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             COSNOTIFYCHANNELADMIN.EVENTCHANNELFACTORY.IMPL               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  with CORBA;

with PortableServer;

package CosNotifyChannelAdmin.EventChannelFactory.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   procedure Create_Channel
      (Self          : access Object;
       Initial_QoS   : CosNotification.QoSProperties;
       Initial_Admin : CosNotification.AdminProperties;
       Id            : out ChannelID;
       Returns       : out CosNotifyChannelAdmin.EventChannel.Ref);

   function Get_All_Channels
      (Self : access Object)
      return CosNotifyChannelAdmin.ChannelIDSeq;

   function Get_Event_Channel
     (Self : access Object;
     Id   : ChannelID)
     return CosNotifyChannelAdmin.EventChannel.Ref;

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

private

   type Event_Channel_Factory_Record;
   type Event_Channel_Factory_Access is access Event_Channel_Factory_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Event_Channel_Factory_Access;
   end record;

end CosNotifyChannelAdmin.EventChannelFactory.Impl;
