------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
-- G L A D E . E V E N T _ C H A N N E L _ A D M I N . I M P L E M E N T A T I O N  --
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

with Ada.Streams;

with GLADE.Event_Channel_Admin.Interface;
use GLADE.Event_Channel_Admin.Interface;

with GLADE.Event_Communication.Interface;
use GLADE.Event_Communication.Interface;

package GLADE.Event_Channel_Admin.Implementation is

   pragma Remote_Types;

   -------------------------
   -- Proxy_Push_Consumer --
   -------------------------

   type Proxy_Push_Consumer is new Interface.Proxy_Push_Consumer with private;

   procedure Connect
     (Consumer : access Proxy_Push_Consumer;
      Supplier : in Push_Supplier_Ref);

   procedure Disconnect
     (Consumer : access Proxy_Push_Consumer);

   procedure Push
     (Consumer : access Proxy_Push_Consumer;
      Event    : in Ada.Streams.Stream_Element_Array);


   -------------------------
   -- Proxy_Pull_Supplier --
   -------------------------

   type Proxy_Pull_Supplier is new Interface.Proxy_Pull_Supplier with private;

   procedure Connect
     (Supplier : access Proxy_Pull_Supplier;
      Consumer : in Pull_Consumer_Ref);

   procedure Disconnect
     (Supplier : access Proxy_Pull_Supplier);

   function Pull
     (Supplier : access Proxy_Pull_Supplier)
      return Ada.Streams.Stream_Element_Array;

   function Try_Pull
     (Supplier : access Proxy_Pull_Supplier)
      return Ada.Streams.Stream_Element_Array;


   -------------------------
   -- Proxy_Pull_Consumer --
   -------------------------

   type Proxy_Pull_Consumer is new Interface.Proxy_Pull_Consumer with private;

   procedure Connect
     (Consumer : access Proxy_Pull_Consumer;
      Supplier : in Pull_Supplier_Ref);

   procedure Disconnect
     (Consumer : access Proxy_Pull_Consumer);


   -------------------------
   -- Proxy_Push_Supplier --
   -------------------------

   type Proxy_Push_Supplier is new Interface.Proxy_Push_Supplier with private;

   procedure Connect
     (Supplier : access Proxy_Push_Supplier;
      Consumer : in Push_Consumer_Ref);

   procedure Disconnect
     (Supplier : access Proxy_Push_Supplier);


   ---------------------
   -- Consummer_Admin --
   ---------------------

   type Consumer_Admin is new Interface.Consumer_Admin with private;

   function Obtain
     (Admin : access Consumer_Admin)
      return Proxy_Push_Supplier_Ref;

   function Obtain
     (Admin : access Consumer_Admin)
      return Proxy_Pull_Supplier_Ref;


   --------------------
   -- Supplier_Admin --
   --------------------

   type Supplier_Admin is new Interface.Supplier_Admin with private;

   function Obtain
     (Admin : access Supplier_Admin)
      return Proxy_Push_Consumer_Ref;

   function Obtain
     (Admin : access Supplier_Admin)
      return Proxy_Pull_Consumer_Ref;


   -------------------
   -- Event_Channel --
   -------------------

   type Event_Channel is new Interface.Event_Channel with private;

   function Create
     return Event_Channel_Ref;

   function For_Consumers
     (Channel : access Event_Channel)
      return Consumer_Admin_Ref;

   function For_Suppliers
     (Channel : access Event_Channel)
      return Supplier_Admin_Ref;

   procedure Destroy
     (Channel : access Event_Channel);

private

   --------------------------------
   -- Proxy_Push_Consumer_Record --
   --------------------------------

   type Proxy_Push_Consumer_Record;
   type Proxy_Push_Consumer_Access is access Proxy_Push_Consumer_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Proxy_Push_Consumer_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Proxy_Push_Consumer_Access);

   for Proxy_Push_Consumer_Access'Read  use Read;
   for Proxy_Push_Consumer_Access'Write use Write;

   type Proxy_Push_Consumer is new Interface.Proxy_Push_Consumer with record
      X : Proxy_Push_Consumer_Access;
   end record;

   --------------------------------
   -- Proxy_Pull_Supplier_Record --
   --------------------------------

   type Proxy_Pull_Supplier_Record;
   type Proxy_Pull_Supplier_Access is access Proxy_Pull_Supplier_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Proxy_Pull_Supplier_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Proxy_Pull_Supplier_Access);

   for Proxy_Pull_Supplier_Access'Read  use Read;
   for Proxy_Pull_Supplier_Access'Write use Write;

   type Proxy_Pull_Supplier is new Interface.Proxy_Pull_Supplier with record
      X : Proxy_Pull_Supplier_Access;
   end record;

   --------------------------------
   -- Proxy_Pull_Consumer_Record --
   --------------------------------

   type Proxy_Pull_Consumer_Record;
   type Proxy_Pull_Consumer_Access is access Proxy_Pull_Consumer_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Proxy_Pull_Consumer_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Proxy_Pull_Consumer_Access);

   for Proxy_Pull_Consumer_Access'Read  use Read;
   for Proxy_Pull_Consumer_Access'Write use Write;

   type Proxy_Pull_Consumer is new Interface.Proxy_Pull_Consumer with record
      X : Proxy_Pull_Consumer_Access;
   end record;

   --------------------------------
   -- Proxy_Push_Supplier_Record --
   --------------------------------

   type Proxy_Push_Supplier_Record;
   type Proxy_Push_Supplier_Access is access Proxy_Push_Supplier_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Proxy_Push_Supplier_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Proxy_Push_Supplier_Access);

   for Proxy_Push_Supplier_Access'Read  use Read;
   for Proxy_Push_Supplier_Access'Write use Write;

   type Proxy_Push_Supplier is new Interface.Proxy_Push_Supplier with record
      X : Proxy_Push_Supplier_Access;
   end record;

   ---------------------------
   -- Consumer_Admin_Record --
   ---------------------------

   type Consumer_Admin_Record;
   type Consumer_Admin_Access is access Consumer_Admin_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Consumer_Admin_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Consumer_Admin_Access);

   for Consumer_Admin_Access'Read  use Read;
   for Consumer_Admin_Access'Write use Write;

   type Consumer_Admin is new Interface.Consumer_Admin with record
      X : Consumer_Admin_Access;
   end record;

   ---------------------------
   -- Supplier_Admin_Record --
   ---------------------------

   type Supplier_Admin_Record;
   type Supplier_Admin_Access is access Supplier_Admin_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Supplier_Admin_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Supplier_Admin_Access);

   for Supplier_Admin_Access'Read  use Read;
   for Supplier_Admin_Access'Write use Write;

   type Supplier_Admin is new Interface.Supplier_Admin with record
      X : Supplier_Admin_Access;
   end record;

   --------------------------
   -- Event_Channel_Record --
   --------------------------

   type Event_Channel_Record;
   type Event_Channel_Access is access Event_Channel_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Event_Channel_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Event_Channel_Access);

   for Event_Channel_Access'Read  use Read;
   for Event_Channel_Access'Write use Write;

   type Event_Channel is new Interface.Event_Channel with record
      X : Event_Channel_Access;
   end record;

end GLADE.Event_Channel_Admin.Implementation;
