------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
-- G L A D E . E V E N T _ C H A N N E L _ A D M I N . I M P L E M E N T A T I O N  --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Streams;               use Ada.Streams;
with GLADE.Lists;
with GLADE.Utils;               use GLADE.Utils;
with GLADE.Event_Communication; use GLADE.Event_Communication;

package body GLADE.Event_Channel_Admin.Implementation is

   type Stream_Element_Access is access Stream_Element_Array;

   --------------------------------------
   -- Local Access To Class Wide Types --
   --------------------------------------

   type Any_Local_Proxy_Push_Consumer is access all Proxy_Push_Consumer;
   type Any_Local_Proxy_Pull_Consumer is access all Proxy_Pull_Consumer;
   type Any_Local_Proxy_Push_Supplier is access all Proxy_Push_Supplier;
   type Any_Local_Proxy_Pull_Supplier is access all Proxy_Pull_Supplier;
   type Any_Local_Consumer_Admin is access all Consumer_Admin;
   type Any_Local_Supplier_Admin is access all Supplier_Admin;
   type Any_Local_Event_Channel is access all Event_Channel;

   -------------------
   -- Proxies Lists --
   -------------------

   package Push_Consumer_Proxies is new Lists (Any_Local_Proxy_Push_Consumer);
   package Pull_Consumer_Proxies is new Lists (Any_Local_Proxy_Pull_Consumer);
   package Push_Supplier_Proxies is new Lists (Any_Local_Proxy_Push_Supplier);
   package Pull_Supplier_Proxies is new Lists (Any_Local_Proxy_Pull_Supplier);

   use Push_Consumer_Proxies;
   use Pull_Consumer_Proxies;
   use Push_Supplier_Proxies;
   use Pull_Supplier_Proxies;

   -------------------------------
   -- Proxy_Pull_Consumer_Engin --
   -------------------------------

   task type Proxy_Pull_Consumer_Engin is
      entry Connect
        (Consumer : Any_Local_Proxy_Pull_Consumer;
         Supplier : Pull_Supplier_Ref);
   end Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Engin_Access is access Proxy_Pull_Consumer_Engin;

   -------------
   -- Proxies --
   -------------

   type Proxy_Push_Consumer_Record is
      record
         This  : Any_Local_Proxy_Push_Consumer;
         Peer  : Push_Supplier_Ref;
         Admin : Any_Local_Supplier_Admin;
         Mutex : Mutex_Type;
      end record;

   type Proxy_Pull_Supplier_Record is
      record
         This  : Any_Local_Proxy_Pull_Supplier;
         Peer  : Pull_Consumer_Ref;
         Admin : Any_Local_Consumer_Admin;
         Stamp : Version_Id;
         Mutex : Mutex_Type;
      end record;

   type Proxy_Pull_Consumer_Record is
      record
         This  : Any_Local_Proxy_Pull_Consumer;
         Peer  : Pull_Supplier_Ref;
         Admin : Any_Local_Supplier_Admin;
         Engin : Proxy_Pull_Consumer_Engin_Access;
         Mutex : Mutex_Type;
      end record;

   type Proxy_Push_Supplier_Record is
      record
         This  : Any_Local_Proxy_Push_Supplier;
         Peer  : Push_Consumer_Ref;
         Admin : Any_Local_Consumer_Admin;
         Mutex : Mutex_Type;
      end record;

   ------------
   -- Admins --
   ------------

   type Consumer_Admin_Record is
      record
         This    : Any_Local_Consumer_Admin;
         Peer    : Any_Local_Supplier_Admin;
         Channel : Any_Local_Event_Channel;
         Pushs   : Push_Supplier_Proxies.List;
         Pulls   : Pull_Supplier_Proxies.List;
         Stack   : Stream_Element_Access;
         Stamp   : Version_Id;
         Watcher : Watcher_Type;
         Mutex   : Mutex_Type;
      end record;

   type Supplier_Admin_Record is
      record
         This    : Any_Local_Supplier_Admin;
         Peer    : Any_Local_Consumer_Admin;
         Channel : Any_Local_Event_Channel;
         Pushs   : Push_Consumer_Proxies.List;
         Pulls   : Pull_Consumer_Proxies.List;
         Mutex   : Mutex_Type;
      end record;

   -------------
   -- Channel --
   -------------

   type Event_Channel_Record is
      record
         This     : Any_Local_Event_Channel;
         Consumer : Any_Local_Consumer_Admin;
         Supplier : Any_Local_Supplier_Admin;
         Mutex    : Mutex_Type;
      end record;

   -----------------------------
   -- Proxy Local Subprograms --
   -----------------------------

   procedure Connect
     (Admin : in  Any_Local_Consumer_Admin;
      Proxy : in  Any_Local_Proxy_Pull_Supplier);

   procedure Connect
     (Admin : in  Any_Local_Consumer_Admin;
      Proxy : in  Any_Local_Proxy_Push_Supplier);

   procedure Connect
     (Admin : in  Any_Local_Supplier_Admin;
      Proxy : in  Any_Local_Proxy_Pull_Consumer);

   procedure Connect
     (Admin : in  Any_Local_Supplier_Admin;
      Proxy : in  Any_Local_Proxy_Push_Consumer);

   procedure Post
     (Admin : in  Any_Local_Consumer_Admin;
      Event : in  Stream_Element_Array);

   procedure Post
     (Admin : in  Any_Local_Supplier_Admin;
      Event : in  Stream_Element_Array);

   procedure Push
     (Supplier : in Any_Local_Proxy_Push_Supplier;
      Event    : in Stream_Element_Array);

   function Pull
     (Consumer : in Any_Local_Proxy_Pull_Consumer)
     return Stream_Element_Array;

   procedure Pull
     (Admin : in  Any_Local_Consumer_Admin;
      Event : out Stream_Element_Access;
      After : in out Version_Id);

   procedure Try_Pull
     (Admin : in  Any_Local_Consumer_Admin;
      Event : out Stream_Element_Access;
      After : in out Version_Id);

   function Try_Pull
     (Consumer : in Any_Local_Proxy_Pull_Consumer)
     return Stream_Element_Array;

   -------------------
   -- Event_Channel --
   -------------------

   -------------------
   -- For_Consumers --
   -------------------

   function For_Consumers
     (Channel : access Event_Channel)
      return Consumer_Admin_Ref
   is
      Consumer : Any_Local_Consumer_Admin;
   begin
      Enter  (Channel.X.Mutex);
      Consumer := Channel.X.Consumer;
      Create (Consumer.X.Mutex);
      Consumer.X.This    := Consumer;
      Consumer.X.Channel := Channel.X.This;
      Consumer.X.Pushs   := Push_Supplier_Proxies.Create;
      Consumer.X.Pulls   := Pull_Supplier_Proxies.Create;
      Consumer.X.Stamp   := No_Version;
      Consumer.X.Stack   := null;
      Leave  (Channel.X.Mutex);
      return Consumer.all'Access;
   end For_Consumers;

   -------------------
   -- For_Consumers --
   -------------------

   function For_Suppliers
     (Channel : access Event_Channel)
      return Supplier_Admin_Ref
   is
   begin
      return Channel.X.Supplier.all'Access;
   end For_Suppliers;

   ------------
   -- Create --
   ------------

   function Create
     return Event_Channel_Ref
   is
      Channel : Any_Local_Event_Channel;
   begin
      Channel                   := new Event_Channel;
      Channel.X                 := new Event_Channel_Record;
      Channel.X.This            := Channel;
      Channel.X.Consumer        := new Consumer_Admin;
      Channel.X.Consumer.X      := new Consumer_Admin_Record;
      Channel.X.Supplier        := new Supplier_Admin;
      Channel.X.Supplier.X      := new Supplier_Admin_Record;
      Channel.X.Supplier.X.Peer := Channel.X.Consumer;
      Channel.X.Consumer.X.Peer := Channel.X.Supplier;
      Create (Channel.X.Mutex);
      return Channel.all'Access;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Channel : access Event_Channel)
   is
   begin
      null;
   end Destroy;

   --------------------
   -- Consumer_Admin --
   --------------------

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Admin : in  Any_Local_Consumer_Admin;
      Proxy : in  Any_Local_Proxy_Pull_Supplier)
   is
   begin
      Enter  (Admin.X.Mutex);
      Append (Admin.X.Pulls, Proxy);
      Leave  (Admin.X.Mutex);
   exception when Constraint_Error =>
      raise Disconnected;
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Admin : in Any_Local_Consumer_Admin;
      Proxy : in Any_Local_Proxy_Push_Supplier)
   is
   begin
      Enter  (Admin.X.Mutex);
      Append (Admin.X.Pushs, Proxy);
      Leave  (Admin.X.Mutex);
   exception when Constraint_Error =>
      raise Disconnected;
   end Connect;

   ------------
   -- Obtain --
   ------------

   function Obtain
     (Admin : access Consumer_Admin)
      return Proxy_Pull_Supplier_Ref
   is
      Supplier : Any_Local_Proxy_Pull_Supplier;
   begin
      Enter (Admin.X.Mutex);
      Supplier         := new Proxy_Pull_Supplier;
      Supplier.X       := new Proxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin.X.This;
      Supplier.X.Stamp := No_Version;
      Create (Supplier.X.Mutex);
      Leave (Admin.X.Mutex);
      return Supplier.all'Access;
   exception when Constraint_Error =>
      raise Disconnected;
   end Obtain;

   ------------
   -- Obtain --
   ------------

   function Obtain
     (Admin : access Consumer_Admin)
      return Proxy_Push_Supplier_Ref
   is
      Supplier : Any_Local_Proxy_Push_Supplier;
   begin
      Enter (Admin.X.Mutex);
      Supplier         := new Proxy_Push_Supplier;
      Supplier.X       := new Proxy_Push_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin.X.This;
      Create (Supplier.X.Mutex);
      Leave (Admin.X.Mutex);
      return Supplier.all'Access;
   exception when Constraint_Error =>
      raise Disconnected;
   end Obtain;

   ----------
   -- Post --
   ----------

   procedure Post
     (Admin : in Any_Local_Consumer_Admin;
      Event : in Stream_Element_Array)
   is
      Supplier : Any_Local_Proxy_Push_Supplier;
   begin
      Enter (Admin.X.Mutex);

      --  Post to Push Consumers
      Head (Admin.X.Pushs);
      while Next (Admin.X.Pushs) loop
         Read (Admin.X.Pushs, Supplier);
         Push (Supplier, Event);
      end loop;

      --  Post to Pull Consumers
      Admin.X.Stack := new Stream_Element_Array'(Event);

      --  Resume Pull Consumers
      Leave (Admin.X.Mutex);
   exception when others =>
      null;
   end Post;

   ----------
   -- Pull --
   ----------

   procedure Pull
     (Admin : in  Any_Local_Consumer_Admin;
      Event : out Stream_Element_Access;
      After : in out Version_Id)
   is
      Version : Version_Id;
   begin
      loop
         Enter (Admin.X.Mutex);

         --  Check this is a new event
         if Admin.X.Stamp /= After then
            Event := new Stream_Element_Array'(Admin.X.Stack.all);
            After := Admin.X.Stamp;
            Leave (Admin.X.Mutex);
            exit;
         end if;

         --  Leave and Suspend until modification
         Lookup (Admin.X.Watcher, Version);
         Leave  (Admin.X.Mutex);
         Differ (Admin.X.Watcher, Version);
      end loop;
   exception when Constraint_Error =>
      raise Disconnected;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Admin : in  Any_Local_Consumer_Admin;
      Event : out Stream_Element_Access;
      After : in out Version_Id)
   is
   begin
      Enter (Admin.X.Mutex);

      --  Check this is a new event
      if Admin.X.Stamp /= After then
         Event := new Stream_Element_Array'(Admin.X.Stack.all);
         After := Admin.X.Stamp;
      else
         Event := null;
      end if;

      Leave (Admin.X.Mutex);
   exception when Constraint_Error =>
      raise Disconnected;
   end Try_Pull;


   --------------------
   -- Supplier_Admin --
   --------------------

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Admin : in  Any_Local_Supplier_Admin;
      Proxy : in  Any_Local_Proxy_Pull_Consumer)
   is
   begin
      Enter  (Admin.X.Mutex);
      Append (Admin.X.Pulls, Proxy);
      Leave  (Admin.X.Mutex);
   exception when Constraint_Error =>
      raise Disconnected;
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Admin : in  Any_Local_Supplier_Admin;
      Proxy : in  Any_Local_Proxy_Push_Consumer)
   is
   begin
      Enter  (Admin.X.Mutex);
      Append (Admin.X.Pushs, Proxy);
      Leave  (Admin.X.Mutex);
   exception when Constraint_Error =>
      raise Disconnected;
   end Connect;

   ------------
   -- Obtain --
   ------------

   function Obtain
     (Admin : access Supplier_Admin)
      return Proxy_Push_Consumer_Ref
   is
      Consumer : Any_Local_Proxy_Push_Consumer;
   begin
      Enter  (Admin.X.Mutex);
      Consumer         := new Proxy_Push_Consumer;
      Consumer.X       := new Proxy_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin.X.This;
      Create (Consumer.X.Mutex);
      Leave  (Admin.X.Mutex);
      return Consumer.all'Access;
   exception when Constraint_Error =>
      raise Disconnected;
   end Obtain;

   ------------
   -- Obtain --
   ------------

   function Obtain
     (Admin : access Supplier_Admin)
      return Proxy_Pull_Consumer_Ref
   is
      Consumer : Any_Local_Proxy_Pull_Consumer;
   begin
      Enter  (Admin.X.Mutex);
      Consumer         := new Proxy_Pull_Consumer;
      Consumer.X       := new Proxy_Pull_Consumer_Record;
      Consumer.X.Engin := new Proxy_Pull_Consumer_Engin;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin.X.This;
      Create (Consumer.X.Mutex);
      Leave  (Admin.X.Mutex);
      return Consumer.all'Access;
   exception when Constraint_Error =>
      raise Disconnected;
   end Obtain;

   ----------
   -- Post --
   ----------

   procedure Post
     (Admin : in  Any_Local_Supplier_Admin;
      Event : in  Stream_Element_Array)
   is
   begin
      Enter (Admin.X.Mutex);
      Post  (Admin.X.Peer, Event);
      Leave (Admin.X.Mutex);
   exception when Constraint_Error =>
      raise Disconnected;
   end Post;

   -------------------------
   -- Proxy_Push_Consumer --
   -------------------------

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Consumer : access Proxy_Push_Consumer;
      Supplier : in Push_Supplier_Ref)
   is
   begin
      Enter (Consumer.X.Mutex);

      --  Check a peer is not already connected
      if Consumer.X.Peer /= null then
         Leave (Consumer.X.Mutex);
         raise Already_Connected;
      end if;

      --  Connect to Admin and check Admin is connected
      Connect (Consumer.X.Admin, Consumer.X.This);
      Consumer.X.Peer := Supplier;
      Leave (Consumer.X.Mutex);
   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Consumer.X.Mutex);
         raise Disconnected;

   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Consumer : access Proxy_Push_Consumer)
   is
   begin
      Enter (Consumer.X.Mutex);

      --  Invalidate Mutex
      Destroy (Consumer.X.Mutex);

      --  Disconnect Push Supplier
      Disconnect (Consumer.X.Peer);
   exception when Constraint_Error =>
      raise Disconnected;
   end Disconnect;

   ----------
   -- Push --
   ----------

   procedure Push
     (Consumer : access Proxy_Push_Consumer;
      Event    : in Stream_Element_Array)
   is
   begin
      Enter (Consumer.X.Mutex);

      --  Check Consumer is connected
      if Consumer.X.Peer = null then
         Leave (Consumer.X.Mutex);
         raise Constraint_Error;
      end if;

      --  Post Event on Admin and check Admin is connected
      Post (Consumer.X.Admin, Event);

      Leave (Consumer.X.Mutex);

   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Consumer.X.Mutex);
         Disconnect (Consumer.X.Peer);
         raise Disconnected;

   end Push;


   -------------------------
   -- Proxy_Pull_Supplier --
   -------------------------

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Supplier : access Proxy_Pull_Supplier;
      Consumer : in Pull_Consumer_Ref)
   is
   begin
      Enter (Supplier.X.Mutex);

      --  Check a peer is not already connected
      if Supplier.X.Peer /= null then
         Leave (Supplier.X.Mutex);
         raise Already_Connected;
      end if;

      --  Connect to Admin and check Admin is connected
      Connect (Supplier.X.Admin, Supplier.X.This);

      Supplier.X.Peer := Consumer;
      Leave (Supplier.X.Mutex);
   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Supplier.X.Mutex);
         raise Disconnected;

   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Supplier : access Proxy_Pull_Supplier)
   is
   begin
      Enter (Supplier.X.Mutex);

      --  Invalidate Mutex
      Destroy (Supplier.X.Mutex);

      --  Disconnect Pull Consumer
      Disconnect (Supplier.X.Peer);
   exception when Constraint_Error =>
      raise Disconnected;
   end Disconnect;

   ----------
   -- Pull --
   ----------

   function Pull
     (Supplier : access Proxy_Pull_Supplier)
     return Stream_Element_Array
   is
      Event : Stream_Element_Access;
   begin
      Enter (Supplier.X.Mutex);

      --  Check Supplier is connected
      if Supplier.X.Peer = null then
         Leave (Supplier.X.Mutex);
         raise Constraint_Error;
      end if;

      --  Pull to Admin and check Admin is connected
      Pull (Supplier.X.Admin, Event, Supplier.X.Stamp);
      declare
         Result : Stream_Element_Array := Event.all;
      begin
         Leave (Supplier.X.Mutex);
         return Result;
      end;
   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Supplier.X.Mutex);
         Disconnect (Supplier.X.Peer);
         raise Disconnected;

   end Pull;

   --------------
   -- Try_Pull --
   --------------

   function Try_Pull
     (Supplier : access Proxy_Pull_Supplier)
     return Stream_Element_Array
   is
      Event : Stream_Element_Access;
   begin
      Enter (Supplier.X.Mutex);

      --  Check Supplier is connected
      if Supplier.X.Peer = null then
         Leave (Supplier.X.Mutex);
         raise Constraint_Error;
      end if;

      --  Try_Pull to Admin and check Admin is connected
      Try_Pull (Supplier.X.Admin, Event, Supplier.X.Stamp);
      declare
         Result : Stream_Element_Array := Event.all;
      begin
         Leave (Supplier.X.Mutex);
         return Result;
      end;

   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Supplier.X.Mutex);
         Disconnect (Supplier.X.Peer);
         raise Disconnected;

   end Try_Pull;


   -------------------------
   -- Proxy_Pull_Consumer --
   -------------------------

   -------------------------------
   -- Proxy_Pull_Consumer_Engin --
   -------------------------------

   task body Proxy_Pull_Consumer_Engin
   is
      This  : Any_Local_Proxy_Pull_Consumer;
      Peer  : Pull_Supplier_Ref;
      Event : Stream_Element_Access;
   begin
      select
         accept Connect
           (Consumer : Any_Local_Proxy_Pull_Consumer;
            Supplier : Pull_Supplier_Ref)
         do
            This := Consumer;
            Peer := Supplier;
         end Connect;
      or
         terminate;
      end select;
      loop
         begin
            Event := new Stream_Element_Array'(Pull (Peer));
         exception when Disconnected =>
            exit;
         end;
         begin
            Enter (This.X.Mutex);
            Post  (This.X.Admin, Event.all);
            Leave (This.X.Mutex);
         exception
            when Constraint_Error =>
               exit;

            when Disconnected =>
               Leave (This.X.Mutex);
               exit;

         end;
      end loop;
   end Proxy_Pull_Consumer_Engin;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Consumer : access Proxy_Pull_Consumer;
      Supplier : in Pull_Supplier_Ref)
   is
   begin
      Enter (Consumer.X.Mutex);

      --  Check a peer is not already connected
      if Consumer.X.Peer /= null then
         Leave (Consumer.X.Mutex);
         raise Already_Connected;
      end if;

      --  Connect to Admin and check Admin is connected
      Connect (Consumer.X.Admin, Consumer.X.This);

      --  Start Pull Consumer Engin
      Consumer.X.Engin.Connect (Consumer.X.This, Supplier);

      Leave (Consumer.X.Mutex);

   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Consumer.X.Mutex);
         raise Disconnected;

   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Consumer : access Proxy_Pull_Consumer)
   is
   begin
      Enter      (Consumer.X.Mutex);
      Destroy    (Consumer.X.Mutex);
      Disconnect (Consumer.X.Peer);
   exception when Constraint_Error =>
      raise Disconnected;
   end Disconnect;

   ----------
   -- Pull --
   ----------

   function Pull
     (Consumer : Any_Local_Proxy_Pull_Consumer)
     return Stream_Element_Array
   is
   begin
      --  This should not be executed
      raise Program_Error;
      return Stream_Element_Array'(1 .. 1 => 0);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   function Try_Pull
     (Consumer : Any_Local_Proxy_Pull_Consumer)
     return Stream_Element_Array
   is
   begin
      --  This should not be executed
      raise Program_Error;
      return Stream_Element_Array'(1 .. 1 => 0);
   end Try_Pull;


   -------------------------
   -- Proxy_Push_Supplier --
   -------------------------

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Supplier : access Proxy_Push_Supplier;
      Consumer : in Push_Consumer_Ref)
   is
   begin
      Enter (Supplier.X.Mutex);

      --  Check a peer is not already connected
      if Supplier.X.Peer /= null then
         Leave (Supplier.X.Mutex);
         raise Already_Connected;
      end if;

      --  Connect to Admin and check Admin is connected
      Connect (Supplier.X.Admin, Supplier.X.This);

      Supplier.X.Peer := Consumer;
      Leave (Supplier.X.Mutex);

   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Supplier.X.Mutex);
         raise Disconnected;

   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Supplier : access Proxy_Push_Supplier)
   is
   begin
      Enter (Supplier.X.Mutex);

      --  Invalidate Mutex
      Destroy (Supplier.X.Mutex);

      --  Disconnect Push Consumer
      Disconnect (Supplier.X.Peer);
   exception when Constraint_Error =>
      raise Disconnected;
   end Disconnect;

   ----------
   -- Push --
   ----------

   procedure Push
     (Supplier : in Any_Local_Proxy_Push_Supplier;
      Event    : in Stream_Element_Array)
   is
   begin
      Enter (Supplier.X.Mutex);

      --  Check Supplier is connected
      if Supplier.X.Peer = null then
         Leave (Supplier.X.Mutex);
         raise Constraint_Error;
      end if;

      --  Push Event on Admin and check Admin is connected
      Push  (Supplier.X.Peer, Event);
      Leave (Supplier.X.Mutex);
   exception
      when Constraint_Error =>
         raise Disconnected;

      when Disconnected =>
         Destroy (Supplier.X.Mutex);
         Disconnect (Supplier.X.Peer);
         raise Disconnected;

   end Push;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Proxy_Push_Consumer_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Proxy_Push_Consumer_Access)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Proxy_Push_Supplier_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Proxy_Push_Supplier_Access)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Proxy_Pull_Consumer_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Proxy_Pull_Consumer_Access)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Proxy_Pull_Supplier_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Proxy_Pull_Supplier_Access)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Consumer_Admin_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Consumer_Admin_Access)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Supplier_Admin_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Supplier_Admin_Access)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out Event_Channel_Access)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in Event_Channel_Access)
   is
   begin
      null;
   end Write;

end GLADE.Event_Channel_Admin.Implementation;
