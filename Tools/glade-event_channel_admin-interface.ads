------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--  G L A D E . E V E N T _ C H A N N E L _ A D M I N . I N T E R F A C E   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with GLADE.Event_Communication;
use GLADE.Event_Communication;

with GLADE.Event_Communication.Interface;
use GLADE.Event_Communication.Interface;

package GLADE.Event_Channel_Admin.Interface is

   pragma Remote_Types;

   -------------------------
   -- Proxy_Push_Consumer --
   -------------------------

   type Proxy_Push_Consumer is
     abstract new Push_Consumer with private;

   type Proxy_Push_Consumer_Ref is
      access all Proxy_Push_Consumer'Class;

   procedure Connect
     (Consumer : access Proxy_Push_Consumer;
      Supplier : in Push_Supplier_Ref)
     is abstract;


   -------------------------
   -- Proxy_Pull_Supplier --
   -------------------------

   type Proxy_Pull_Supplier is
     abstract new Pull_Supplier with private;

   type Proxy_Pull_Supplier_Ref is
      access all Proxy_Pull_Supplier'Class;

   procedure Connect
     (Supplier : access Proxy_Pull_Supplier;
      Consumer : in Pull_Consumer_Ref)
     is abstract;


   -------------------------
   -- Proxy_Pull_Consumer --
   -------------------------

   type Proxy_Pull_Consumer is
     abstract new Pull_Consumer with private;

   type Proxy_Pull_Consumer_Ref is
      access all Proxy_Pull_Consumer'Class;

   procedure Connect
     (Consumer : access Proxy_Pull_Consumer;
      Supplier : in Pull_Supplier_Ref)
     is abstract;


   -------------------------
   -- Proxy_Push_Supplier --
   -------------------------

   type Proxy_Push_Supplier is
     abstract new Push_Supplier with private;

   type Proxy_Push_Supplier_Ref is
      access all Proxy_Push_Supplier'Class;

   procedure Connect
     (Supplier : access Proxy_Push_Supplier;
      Consumer : in Push_Consumer_Ref);


   ---------------------
   -- Consummer_Admin --
   ---------------------

   type Consumer_Admin is abstract tagged limited private;
   type Consumer_Admin_Ref is access all Consumer_Admin'Class;

   function Obtain
     (Admin : access Consumer_Admin)
      return Proxy_Push_Supplier_Ref
     is abstract;

   function Obtain
     (Admin : access Consumer_Admin)
      return Proxy_Pull_Supplier_Ref
      is abstract;


   --------------------
   -- Supplier_Admin --
   --------------------

   type Supplier_Admin is abstract tagged limited private;
   type Supplier_Admin_Ref is access all Supplier_Admin'Class;

   function Obtain
     (Admin : access Supplier_Admin)
      return Proxy_Push_Consumer_Ref
      is abstract;

   function Obtain
     (Admin : access Supplier_Admin)
      return Proxy_Pull_Consumer_Ref
      is abstract;


   -------------------
   -- Event_Channel --
   -------------------

   type Event_Channel is abstract tagged limited private;
   type Event_Channel_Ref is access all Event_Channel'Class;

   function For_Consumers
     (Channel : access Event_Channel)
      return Consumer_Admin_Ref
      is abstract;

   function For_Suppliers
     (Channel : access Event_Channel)
      return Supplier_Admin_Ref
      is abstract;

   procedure Destroy
     (Channel : access Event_Channel)
      is abstract;

private

   type Proxy_Push_Consumer is
     abstract new Push_Consumer with null record;

   type Proxy_Pull_Supplier is
     abstract new Pull_Supplier with null record;

   type Proxy_Pull_Consumer is
     abstract new Pull_Consumer with null record;

   type Proxy_Push_Supplier is
     abstract new Push_Supplier with null record;

   type Consumer_Admin is abstract tagged limited null record;
   type Supplier_Admin is abstract tagged limited null record;

   type Event_Channel is abstract tagged limited null record;

end GLADE.Event_Channel_Admin.Interface;
