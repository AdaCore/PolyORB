------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--       C O S E V E N T C O M M . P U L L C O N S U M E R . I M P L        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PortableServer;

with CosEventChannelAdmin.ProxyPullSupplier;

package CosEventComm.PullConsumer.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Disconnect_Pull_Consumer
     (Self : access Object);
   --  Call by proxy to disconnect

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Pull_Supplier
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPullSupplier.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   function Pull (Self : access Object) return CORBA.Any;
   --  Call by application to consume an event

   procedure Try_Pull
     (Self    : access Object;
      Done    : out CORBA.Boolean;
      Returns : out CORBA.Any);
   --  Call by application to try to consume an event

private

   type Pull_Consumer_Record;
   type Pull_Consumer_Access is access Pull_Consumer_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Pull_Consumer_Access;
      end record;

end CosEventComm.PullConsumer.Impl;
