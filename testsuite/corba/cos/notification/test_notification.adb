------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T _ N O T I F I C A T I O N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  Test PolyORB COS notification capabilities.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with Ada.Integer_Text_IO;

with CORBA;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer;

with CosNaming.BindingIterator;

with CosNaming.NamingContext.Impl;
with CosNaming.NamingContext.Helper;

with CosNotification;
with CosNotification.Helper;

with CosNotifyChannelAdmin;

with CosNotifyChannelAdmin.ConsumerAdmin;
with CosNotifyChannelAdmin.SupplierAdmin;

with CosNotifyChannelAdmin.EventChannel.Helper;

with CosNotifyChannelAdmin.EventChannelFactory;
with CosNotifyChannelAdmin.EventChannelFactory.Impl;

with CosNotifyChannelAdmin.ProxyConsumer;
with CosNotifyChannelAdmin.ProxySupplier;

with CosNotifyChannelAdmin.ProxyPullConsumer.Helper;
with CosNotifyChannelAdmin.ProxyPushConsumer.Helper;

with CosNotifyChannelAdmin.ProxyPullSupplier.Helper;
with CosNotifyChannelAdmin.ProxyPushSupplier.Helper;

with CosNotifyChannelAdmin.SequenceProxyPullConsumer.Helper;
with CosNotifyChannelAdmin.SequenceProxyPushConsumer.Helper;

with CosNotifyChannelAdmin.SequenceProxyPullSupplier.Helper;
with CosNotifyChannelAdmin.SequenceProxyPushSupplier.Helper;

with CosNotifyChannelAdmin.StructuredProxyPullConsumer.Helper;
with CosNotifyChannelAdmin.StructuredProxyPushConsumer.Helper;

with CosNotifyChannelAdmin.StructuredProxyPullSupplier.Helper;
with CosNotifyChannelAdmin.StructuredProxyPushSupplier.Helper;

with CosNotifyComm.PullConsumer.Helper;
with CosNotifyComm.PullConsumer.Impl;

with CosNotifyComm.PushConsumer.Helper;
with CosNotifyComm.PushConsumer.Impl;

with CosNotifyComm.PullSupplier.Helper;
with CosNotifyComm.PullSupplier.Impl;

with CosNotifyComm.PushSupplier.Helper;
with CosNotifyComm.PushSupplier.Impl;

with CosNotifyComm.SequencePullConsumer.Helper;
with CosNotifyComm.SequencePullConsumer.Impl;

with CosNotifyComm.SequencePushConsumer.Helper;
with CosNotifyComm.SequencePushConsumer.Impl;

with CosNotifyComm.SequencePullSupplier.Helper;
with CosNotifyComm.SequencePullSupplier.Impl;

with CosNotifyComm.SequencePushSupplier.Helper;
with CosNotifyComm.SequencePushSupplier.Impl;

with CosNotifyComm.StructuredPullConsumer.Helper;
with CosNotifyComm.StructuredPullConsumer.Impl;

with CosNotifyComm.StructuredPushConsumer.Helper;
with CosNotifyComm.StructuredPushConsumer.Impl;

with CosNotifyComm.StructuredPullSupplier.Helper;
with CosNotifyComm.StructuredPullSupplier.Impl;

with CosNotifyComm.StructuredPushSupplier.Helper;
with CosNotifyComm.StructuredPushSupplier.Impl;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Any;

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with Auto_Print;
--  Auxiliary code to output pushconsumer incoming messages.

with Menu;
--  From COS Naming, provide text interface.

procedure Test_Notification is

   use Ada.Exceptions;
   use Ada.Integer_Text_IO;

   use Auto_Print;
   use Menu;

   use PortableServer;

   use CORBA;

   use CosNaming;

   use CosNotification;

   use CosNotifyChannelAdmin;
   use CosNotifyComm;
   use CosNotifyComm.PushConsumer.Impl;

   use IDL_SEQUENCE_CosNotifyChannelAdmin_AdminID;
   use IDL_SEQUENCE_CosNotifyChannelAdmin_ChannelID;
   use IDL_SEQUENCE_CosNotifyChannelAdmin_ProxyID;

   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use IDL_SEQUENCE_CosNotification_StructuredEvent;

   use PolyORB.Any;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;

   type Command is
     (Help,
      Quit,
      Run,
      Sleep,
      Connect,
      Consume,
      TryConsume,
      Produce,
      AutoDisplay,
      Create);

   Syntax_Error : exception;

   Help_Messages : constant array (Command) of String_Access
     := (Help        => +(ASCII.HT & "print this message"),
         Quit        => +(ASCII.HT & "quit this shell"),
         Run         => +(ASCII.HT
                          & "run <file of commands from this language>"),
         Sleep       => +(ASCII.HT & "sleep <seconds>"),
         Create      => +(ASCII.HT & "create <kind> <entity>"),
         Connect     => +(ASCII.HT & "connect <entity> to <channel>"),
         Consume     => +(ASCII.HT & "consume in <entity>"),
         TryConsume  => +("tryconsume in <entity>"),
         AutoDisplay => +("autodisplay <pushconsumer>"),
         Produce     => +(ASCII.HT
                          & "produce <string> in <entity> [<N> times]"));

   type Entity_Kind is
     (K_Channel,
      K_PullConsumer,
      K_PullSupplier,
      K_PushConsumer,
      K_PushSupplier,
      K_SequencePullConsumer,
      K_SequencePullSupplier,
      K_SequencePushConsumer,
      K_SequencePushSupplier,
      K_StructuredPullConsumer,
      K_StructuredPullSupplier,
      K_StructuredPushConsumer,
      K_StructuredPushSupplier);

   Image : array (Entity_Kind) of CosNaming.Istring
     := (K_Channel      => CosNaming.To_CORBA_String (K_Channel'Img),
         K_PullConsumer => CosNaming.To_CORBA_String (K_PullConsumer'Img),
         K_PullSupplier => CosNaming.To_CORBA_String (K_PullSupplier'Img),
         K_PushConsumer => CosNaming.To_CORBA_String (K_PushConsumer'Img),
         K_PushSupplier => CosNaming.To_CORBA_String (K_PushSupplier'Img),
         K_SequencePullConsumer
                  => CosNaming.To_CORBA_String (K_SequencePullConsumer'Img),
         K_SequencePullSupplier
                  => CosNaming.To_CORBA_String (K_SequencePullSupplier'Img),
         K_SequencePushConsumer
                  => CosNaming.To_CORBA_String (K_SequencePushConsumer'Img),
         K_SequencePushSupplier
                  => CosNaming.To_CORBA_String (K_SequencePushSupplier'Img),
         K_StructuredPushConsumer
                  => CosNaming.To_CORBA_String (K_StructuredPushConsumer'Img),
         K_StructuredPushSupplier
                  => CosNaming.To_CORBA_String (K_StructuredPushSupplier'Img),
         K_StructuredPullConsumer
                  => CosNaming.To_CORBA_String (K_StructuredPullConsumer'Img),
         K_StructuredPullSupplier
                  => CosNaming.To_CORBA_String (K_StructuredPullSupplier'Img));
   Ctx : NamingContext.Ref;

   --------------------
   -- Connect_Entity --
   --------------------

   procedure Connect_Entity
     (Entity  : in CORBA.Object.Ref;
      Kind    : in Entity_Kind;
      Channel : in CosNotifyChannelAdmin.EventChannel.Ref);

   procedure Connect_Entity
     (Entity  : in CORBA.Object.Ref;
      Kind    : in Entity_Kind;
      Channel : in CosNotifyChannelAdmin.EventChannel.Ref)
   is
      O : CORBA.Impl.Object_Ptr;
   begin
      case Kind is

         when K_PullConsumer =>
            declare
               A      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
               E      : CosNotifyComm.PullConsumer.Ref;
               P      : CosNotifyChannelAdmin.ProxySupplier.Ref;
               PP_Ref : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType := ANY_EVENT;
               CID    : CosNotifyChannelAdmin.AdminID;
               PID    : CosNotifyChannelAdmin.ProxyID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_consumers (Channel, My_Op, CID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pushconsumer

               CID := CosNotifyChannelAdmin.ConsumerAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of consumer admin is : ");
               Ada.Integer_Text_IO.Put (Integer (CID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.ConsumerAdmin.
               obtain_notification_pull_supplier (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.ProxyPullSupplier.Helper.To_Ref
                         (P);

               E := CosNotifyComm.PullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PullConsumer.Impl.Connect_Any_Proxy_Pull_Supplier
               (PullConsumer.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_PushConsumer =>
            declare
               A      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
               E      : CosNotifyComm.PushConsumer.Ref;
               P      : CosNotifyChannelAdmin.ProxySupplier.Ref;
               PP_Ref : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType := ANY_EVENT;
               CID    : CosNotifyChannelAdmin.AdminID;
               PID    : CosNotifyChannelAdmin.ProxyID;
            begin
               A := CosNotifyChannelAdmin.EventChannel.
                    Get_default_consumer_admin (Channel);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pullconsumer

               CID := CosNotifyChannelAdmin.ConsumerAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of consumer admin is : ");
               Ada.Integer_Text_IO.Put (Integer (CID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.ConsumerAdmin.
               obtain_notification_push_supplier (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.ProxyPushSupplier.Helper.To_Ref
                         (P);

               E := CosNotifyComm.PushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PushConsumer.Impl.Connect_Any_Proxy_Push_Supplier
               (PushConsumer.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_PullSupplier =>
            declare
               A      : CosNotifyChannelAdmin.SupplierAdmin.Ref;
               E      : CosNotifyComm.PullSupplier.Ref;
               P      : CosNotifyChannelAdmin.ProxyConsumer.Ref;
               PP_Ref : CosNotifyChannelAdmin.ProxyPullConsumer.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType := ANY_EVENT;
               PID    : CosNotifyChannelAdmin.ProxyID;
               SID    : CosNotifyChannelAdmin.AdminID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_suppliers (Channel, My_Op, SID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pushsupplier

               SID := CosNotifyChannelAdmin.SupplierAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of supplier admin is : ");
               Ada.Integer_Text_IO.Put (Integer (SID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.SupplierAdmin.
               obtain_notification_pull_consumer (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.ProxyPullConsumer.Helper.To_Ref
                         (P);

               E := CosNotifyComm.PullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PullSupplier.Impl.Connect_Any_Proxy_Pull_Consumer
               (PullSupplier.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_PushSupplier =>
            declare
               A      : CosNotifyChannelAdmin.SupplierAdmin.Ref;
               E      : CosNotifyComm.PushSupplier.Ref;
               P      : CosNotifyChannelAdmin.ProxyConsumer.Ref;
               PP_Ref : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType := ANY_EVENT;
               PID    : CosNotifyChannelAdmin.ProxyID;
               SID    : CosNotifyChannelAdmin.AdminID;
            begin
               A := CosNotifyChannelAdmin.EventChannel.
                    Get_default_supplier_admin (Channel);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pullsupplier

               SID := CosNotifyChannelAdmin.SupplierAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of supplier admin is : ");
               Ada.Integer_Text_IO.Put (Integer (SID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.SupplierAdmin.
               obtain_notification_push_consumer (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.ProxyPushConsumer.Helper.To_Ref
                         (P);

               E := CosNotifyComm.PushSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PushSupplier.Impl.Connect_Any_Proxy_Push_Consumer
               (PushSupplier.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_SequencePullConsumer =>
            declare
               A      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
               E      : CosNotifyComm.SequencePullConsumer.Ref;
               P      : CosNotifyChannelAdmin.ProxySupplier.Ref;
               PP_Ref : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := SEQUENCE_EVENT;
               CID    : CosNotifyChannelAdmin.AdminID;
               PID    : CosNotifyChannelAdmin.ProxyID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_consumers (Channel, My_Op, CID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pushconsumer

               CID := CosNotifyChannelAdmin.ConsumerAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of consumer admin is : ");
               Ada.Integer_Text_IO.Put (Integer (CID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.ConsumerAdmin.
               obtain_notification_pull_supplier (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.SequenceProxyPullSupplier.
                         Helper.To_Ref (P);

               E := CosNotifyComm.SequencePullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               SequencePullConsumer.Impl.Connect_Sequence_Proxy_Pull_Supplier
               (SequencePullConsumer.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_SequencePullSupplier =>
            declare
               A      : CosNotifyChannelAdmin.SupplierAdmin.Ref;
               E      : CosNotifyComm.SequencePullSupplier.Ref;
               P      : CosNotifyChannelAdmin.ProxyConsumer.Ref;
               PP_Ref : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := SEQUENCE_EVENT;
               PID    : CosNotifyChannelAdmin.ProxyID;
               SID    : CosNotifyChannelAdmin.AdminID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_suppliers (Channel, My_Op, SID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pullsupplier

               SID := CosNotifyChannelAdmin.SupplierAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of supplier admin is : ");
               Ada.Integer_Text_IO.Put (Integer (SID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.SupplierAdmin.
               obtain_notification_pull_consumer (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.SequenceProxyPullConsumer.
                         Helper.To_Ref (P);

               E := CosNotifyComm.SequencePullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               SequencePullSupplier.Impl.Connect_Sequence_Proxy_Pull_Consumer
               (SequencePullSupplier.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_SequencePushConsumer =>
            declare
               A      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
               E      : CosNotifyComm.SequencePushConsumer.Ref;
               P      : CosNotifyChannelAdmin.ProxySupplier.Ref;
               PP_Ref : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := SEQUENCE_EVENT;
               CID    : CosNotifyChannelAdmin.AdminID;
               PID    : CosNotifyChannelAdmin.ProxyID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_consumers (Channel, My_Op, CID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pushconsumer

               CID := CosNotifyChannelAdmin.ConsumerAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of consumer admin is : ");
               Ada.Integer_Text_IO.Put (Integer (CID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.ConsumerAdmin.
               obtain_notification_push_supplier (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.SequenceProxyPushSupplier.
                         Helper.To_Ref (P);

               E := CosNotifyComm.SequencePushConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               SequencePushConsumer.Impl.
               Connect_Sequence_Proxy_Push_Supplier
               (SequencePushConsumer.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_SequencePushSupplier =>
            declare
               A      : CosNotifyChannelAdmin.SupplierAdmin.Ref;
               E      : CosNotifyComm.SequencePushSupplier.Ref;
               P      : CosNotifyChannelAdmin.ProxyConsumer.Ref;
               PP_Ref : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := SEQUENCE_EVENT;
               PID    : CosNotifyChannelAdmin.ProxyID;
               SID    : CosNotifyChannelAdmin.AdminID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_suppliers (Channel, My_Op, SID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pullsupplier

               SID := CosNotifyChannelAdmin.SupplierAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of supplier admin is : ");
               Ada.Integer_Text_IO.Put (Integer (SID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.SupplierAdmin.
               obtain_notification_push_consumer (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.SequenceProxyPushConsumer.
                         Helper.To_Ref (P);

               E := CosNotifyComm.SequencePushSupplier.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               SequencePushSupplier.Impl.
               Connect_Sequence_Proxy_Push_Consumer
               (SequencePushSupplier.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_StructuredPullConsumer =>
            declare
               A      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
               E      : CosNotifyComm.StructuredPullConsumer.Ref;
               P      : CosNotifyChannelAdmin.ProxySupplier.Ref;
               PP_Ref : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := STRUCTURED_EVENT;
               CID    : CosNotifyChannelAdmin.AdminID;
               PID    : CosNotifyChannelAdmin.ProxyID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_consumers (Channel, My_Op, CID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pushconsumer

               CID := CosNotifyChannelAdmin.ConsumerAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of consumer admin is : ");
               Ada.Integer_Text_IO.Put (Integer (CID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.ConsumerAdmin.
               obtain_notification_pull_supplier (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.StructuredProxyPullSupplier.
                         Helper.To_Ref (P);

               E := CosNotifyComm.StructuredPullConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               StructuredPullConsumer.Impl.
               Connect_Structured_Proxy_Pull_Supplier
               (StructuredPullConsumer.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_StructuredPullSupplier =>
            declare
               A      : CosNotifyChannelAdmin.SupplierAdmin.Ref;
               E      : CosNotifyComm.StructuredPullSupplier.Ref;
               P      : CosNotifyChannelAdmin.ProxyConsumer.Ref;
               PP_Ref : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := STRUCTURED_EVENT;
               PID    : CosNotifyChannelAdmin.ProxyID;
               SID    : CosNotifyChannelAdmin.AdminID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_suppliers (Channel, My_Op, SID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pullsupplier

               SID := CosNotifyChannelAdmin.SupplierAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of supplier admin is : ");
               Ada.Integer_Text_IO.Put (Integer (SID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.SupplierAdmin.
               obtain_notification_pull_consumer (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.StructuredProxyPullConsumer.
                         Helper.To_Ref (P);

               E := CosNotifyComm.StructuredPullSupplier.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               StructuredPullSupplier.Impl.
               Connect_Structured_Proxy_Pull_Consumer
               (StructuredPullSupplier.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_StructuredPushConsumer =>
            declare
               A      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
               E      : CosNotifyComm.StructuredPushConsumer.Ref;
               P      : CosNotifyChannelAdmin.ProxySupplier.Ref;
               PP_Ref : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := STRUCTURED_EVENT;
               CID    : CosNotifyChannelAdmin.AdminID;
               PID    : CosNotifyChannelAdmin.ProxyID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_consumers (Channel, My_Op, CID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pushconsumer

               CID := CosNotifyChannelAdmin.ConsumerAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of consumer admin is : ");
               Ada.Integer_Text_IO.Put (Integer (CID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.ConsumerAdmin.
               obtain_notification_push_supplier (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.StructuredProxyPushSupplier.
                         Helper.To_Ref (P);

               E := CosNotifyComm.StructuredPushConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               StructuredPushConsumer.Impl.
               Connect_Structured_Proxy_Push_Supplier
               (StructuredPushConsumer.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_StructuredPushSupplier =>
            declare
               A      : CosNotifyChannelAdmin.SupplierAdmin.Ref;
               E      : CosNotifyComm.StructuredPushSupplier.Ref;
               P      : CosNotifyChannelAdmin.ProxyConsumer.Ref;
               PP_Ref : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
               Ctype  : constant CosNotifyChannelAdmin.ClientType
                      := STRUCTURED_EVENT;
               PID    : CosNotifyChannelAdmin.ProxyID;
               SID    : CosNotifyChannelAdmin.AdminID;
               My_Op  : constant CosNotifyChannelAdmin.InterFilterGroupOperator
                        := AND_OP;
            begin
               CosNotifyChannelAdmin.EventChannel.
               new_for_suppliers (Channel, My_Op, SID, A);
               --  There are 2 methods for creating a consumer/supplier admin
               --  one is used here and the other is used for pullsupplier

               SID := CosNotifyChannelAdmin.SupplierAdmin.Get_MyID (A);
               Ada.Text_IO.Put ("The ID of supplier admin is : ");
               Ada.Integer_Text_IO.Put (Integer (SID), 3);
               Ada.Text_IO.New_Line;

               CosNotifyChannelAdmin.SupplierAdmin.
               obtain_notification_push_consumer (A, Ctype, PID, P);
               PP_Ref := CosNotifyChannelAdmin.StructuredProxyPushConsumer.
                         Helper.To_Ref (P);

               E := CosNotifyComm.StructuredPushSupplier.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));

               StructuredPushSupplier.Impl.
               Connect_Structured_Proxy_Push_Consumer
               (StructuredPushSupplier.Impl.Object_Ptr (O), PP_Ref);
            end;

         when K_Channel =>
            raise Syntax_Error;

      end case;
   end Connect_Entity;

   -------------------
   -- Consume_Event --
   -------------------

   procedure Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind);

   procedure Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind)
   is
      O : CORBA.Impl.Object_Ptr;
      A : CORBA.Any;
   begin
      case Kind is

         when K_PullConsumer =>
            declare
               C            : CosNotifyComm.PullConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
            begin
               C := CosNotifyComm.PullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               A := CosNotifyComm.PullConsumer.Impl.Pull
                   (PullConsumer.Impl.Object_Ptr (O));
               declare
                  Tc : constant CORBA.TypeCode.Object := CORBA.Get_Type (A);
                  MyKind : constant CORBA.TCKind := CORBA.TypeCode.Kind (Tc);
               begin
                  if MyKind = PolyORB.Any.Tk_Struct then
                     Ada.Text_IO.Put_Line ("TypeCode Kind : Tk_Struct");
                     Notification := CosNotification.Helper.From_Any (A);
                     Ada.Text_IO.Put_Line (CORBA.To_Standard_String
                           (CORBA.From_Any (Notification.remainder_of_body)));
                  elsif MyKind = PolyORB.Any.Tk_String then
                     Ada.Text_IO.Put_Line ("TypeCode Kind : String");
                     Ada.Text_IO.Put_Line
                       (CORBA.To_Standard_String (From_Any (A)));
                  else
                     Ada.Text_IO.Put_Line ("Error : Event type Not Known");
                  end if;
               end;
            end;

         when K_PushConsumer =>
            declare
               C            : CosNotifyComm.PushConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
            begin
               C := CosNotifyComm.PushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               A := CosNotifyComm.PushConsumer.Impl.Pull
                 (PushConsumer.Impl.Object_Ptr (O));
               declare
                  Tc : constant CORBA.TypeCode.Object := CORBA.Get_Type (A);
                  MyKind : constant CORBA.TCKind := CORBA.TypeCode.Kind (Tc);
               begin
                  if MyKind = PolyORB.Any.Tk_Struct then
                     Ada.Text_IO.Put_Line ("TypeCode Kind : Tk_Struct");
                     Notification := CosNotification.Helper.From_Any (A);
                     Ada.Text_IO.Put_Line (CORBA.To_Standard_String
                           (CORBA.From_Any (Notification.remainder_of_body)));
                  elsif MyKind = PolyORB.Any.Tk_String then
                     Ada.Text_IO.Put_Line ("TypeCode Kind : String");
                     Ada.Text_IO.Put_Line
                       (CORBA.To_Standard_String (From_Any (A)));
                  else
                     Ada.Text_IO.Put_Line ("Error : Event type Not Known");
                  end if;
               end;
            end;

         when K_SequencePushConsumer =>
            declare
               C            : CosNotifyComm.SequencePushConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
               MyStructEvent_Seq : CosNotification.EventBatch;
               SeqLen       : Integer;
            begin
               C := CosNotifyComm.SequencePushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               MyStructEvent_Seq := CosNotifyComm.SequencePushConsumer.Impl.
                               Pull (SequencePushConsumer.Impl.Object_Ptr (O));
               SeqLen := Length (MyStructEvent_Seq);
               for Index in 1 .. SeqLen loop
                  Notification := Get_Element (MyStructEvent_Seq, Index);
                  Ada.Text_IO.Put_Line ("Main components of structured event");
                  MyDomainName :=
                  Notification.header.fixed_header.event_type.domain_name;
                  Ada.Text_IO.Put ("My Domain : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
                  Ada.Text_IO.New_Line;

                  MyTypeName :=
                  Notification.header.fixed_header.event_type.type_name;
                  Ada.Text_IO.Put ("My Type : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
                  Ada.Text_IO.New_Line;

                  MyName :=  Notification.header.fixed_header.event_name;
                  Ada.Text_IO.Put ("My Name : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
                  Ada.Text_IO.New_Line;

                  MyBody :=  Notification.remainder_of_body;
                  Ada.Text_IO.Put ("Event Body : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String
                               (CORBA.From_Any (MyBody)));
                  Ada.Text_IO.New_Line;
               end loop;
            end;

         when K_SequencePullConsumer =>
            declare
               C            : CosNotifyComm.SequencePullConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
               MyStructEvent_Seq : CosNotification.EventBatch;
               SeqLen       : Integer;
               Num_Event    : constant CORBA.Long := 2;
            begin
               C := CosNotifyComm.SequencePullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               MyStructEvent_Seq :=
                  CosNotifyComm.SequencePullConsumer.Impl.Pull
                     (SequencePullConsumer.Impl.Object_Ptr (O), Num_Event);
               SeqLen := Length (MyStructEvent_Seq);
               for Index in 1 .. SeqLen loop
                  Notification := Get_Element (MyStructEvent_Seq, Index);
                  Ada.Text_IO.Put_Line ("Main components of structured event");
                  MyDomainName :=
                  Notification.header.fixed_header.event_type.domain_name;
                  Ada.Text_IO.Put ("My Domain : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
                  Ada.Text_IO.New_Line;

                  MyTypeName :=
                  Notification.header.fixed_header.event_type.type_name;
                  Ada.Text_IO.Put ("My Type : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
                  Ada.Text_IO.New_Line;

                  MyName :=  Notification.header.fixed_header.event_name;
                  Ada.Text_IO.Put ("My Name : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
                  Ada.Text_IO.New_Line;

                  MyBody :=  Notification.remainder_of_body;
                  Ada.Text_IO.Put ("Event Body : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String
                               (CORBA.From_Any (MyBody)));
                  Ada.Text_IO.New_Line;
               end loop;
            end;

         when K_StructuredPullConsumer =>
            declare
               C            : CosNotifyComm.StructuredPullConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
            begin
               C := CosNotifyComm.StructuredPullConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               Notification := CosNotifyComm.StructuredPullConsumer.Impl.Pull
                               (StructuredPullConsumer.Impl.Object_Ptr (O));
               Ada.Text_IO.Put_Line ("Main components of structured event");
               MyDomainName :=
               Notification.header.fixed_header.event_type.domain_name;
               Ada.Text_IO.Put ("My Domain : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
               Ada.Text_IO.New_Line;

               MyTypeName :=
               Notification.header.fixed_header.event_type.type_name;
               Ada.Text_IO.Put ("My Type : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
               Ada.Text_IO.New_Line;

               MyName :=  Notification.header.fixed_header.event_name;
               Ada.Text_IO.Put ("My Name : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
               Ada.Text_IO.New_Line;

               MyBody :=  Notification.remainder_of_body;
               Ada.Text_IO.Put ("Event Body : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String
                               (CORBA.From_Any (MyBody)));
               Ada.Text_IO.New_Line;
            end;

         when K_StructuredPushConsumer =>
            declare
               C            : CosNotifyComm.StructuredPushConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
            begin
               C := CosNotifyComm.StructuredPushConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               Notification := CosNotifyComm.StructuredPushConsumer.Impl.Pull
                               (StructuredPushConsumer.Impl.Object_Ptr (O));
               Ada.Text_IO.Put_Line ("Main components of structured event");
               MyDomainName :=
               Notification.header.fixed_header.event_type.domain_name;
               Ada.Text_IO.Put ("My Domain : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
               Ada.Text_IO.New_Line;

               MyTypeName :=
               Notification.header.fixed_header.event_type.type_name;
               Ada.Text_IO.Put ("My Type : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
               Ada.Text_IO.New_Line;

               MyName :=  Notification.header.fixed_header.event_name;
               Ada.Text_IO.Put ("My Name : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
               Ada.Text_IO.New_Line;

               MyBody :=  Notification.remainder_of_body;
               Ada.Text_IO.Put ("Event Body : ");
               Ada.Text_IO.Put (CORBA.To_Standard_String
                               (CORBA.From_Any (MyBody)));
               Ada.Text_IO.New_Line;
            end;

         when others =>
            null;
      end case;
   end Consume_Event;

   -----------------------
   -- Try_Consume_Event --
   -----------------------

   procedure Try_Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind);

   procedure Try_Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind)
   is
      O : CORBA.Impl.Object_Ptr;
      A : CORBA.Any;
      B : CORBA.Boolean;
      Notification : CosNotification.StructuredEvent;
   begin
      case Kind is

         when K_PullConsumer =>
            declare
               C : CosNotifyComm.PullConsumer.Ref;
            begin
               C := CosNotifyComm.PullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               CosNotifyComm.PullConsumer.Impl.Try_Pull
                 (CosNotifyComm.PullConsumer.Impl.Object_Ptr (O), B, A);
               if B then
                  declare
                     Tc     : constant CORBA.TypeCode.Object
                            := CORBA.Get_Type (A);
                     MyKind : constant CORBA.TCKind
                            := CORBA.TypeCode.Kind (Tc);
                  begin
                     if MyKind = PolyORB.Any.Tk_Struct then
                        Ada.Text_IO.Put_Line ("TypeCode Kind : Tk_Struct");
                        Notification := CosNotification.Helper.From_Any (A);
                        Ada.Text_IO.Put_Line (CORBA.To_Standard_String
                           (CORBA.From_Any (Notification.remainder_of_body)));
                     elsif MyKind = PolyORB.Any.Tk_String then
                        Ada.Text_IO.Put_Line ("TypeCode Kind : String");
                        Ada.Text_IO.Put_Line
                          (CORBA.To_Standard_String (From_Any (A)));
                     else
                        Ada.Text_IO.Put_Line ("Error : Event type Not Known");
                     end if;
                  end;
               else
                  Ada.Text_IO.Put_Line ("Nothing to consume!!!");
               end if;
            end;

         when K_PushConsumer =>
            declare
               C : CosNotifyComm.PushConsumer.Ref;
            begin
               C := CosNotifyComm.PushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               CosNotifyComm.PushConsumer.Impl.Try_Pull
                 (CosNotifyComm.PushConsumer.Impl.Object_Ptr (O), B, A);
               if B then
                  declare
                     Tc     : constant CORBA.TypeCode.Object
                            := CORBA.Get_Type (A);
                     MyKind : constant CORBA.TCKind
                            := CORBA.TypeCode.Kind (Tc);
                  begin
                     if MyKind = PolyORB.Any.Tk_Struct then
                        Ada.Text_IO.Put_Line ("TypeCode Kind : Tk_Struct");
                        Notification := CosNotification.Helper.From_Any (A);
                        Ada.Text_IO.Put_Line (CORBA.To_Standard_String
                           (CORBA.From_Any (Notification.remainder_of_body)));
                     elsif MyKind = PolyORB.Any.Tk_String then
                        Ada.Text_IO.Put_Line ("TypeCode Kind : String");
                        Ada.Text_IO.Put_Line
                          (CORBA.To_Standard_String (From_Any (A)));
                     else
                        Ada.Text_IO.Put_Line ("Error : Event type Not Known");
                     end if;
                  end;
               else
                  Ada.Text_IO.Put_Line ("Nothing to consume!!!");
               end if;
            end;

         when K_SequencePushConsumer =>
            declare
               C            : CosNotifyComm.SequencePushConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
               MyStructEvent_Seq : CosNotification.EventBatch;
               SeqLen       : Integer;
            begin
               C := CosNotifyComm.SequencePushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               CosNotifyComm.SequencePushConsumer.Impl.Try_Pull
                 (SequencePushConsumer.Impl.Object_Ptr (O),
                  B, MyStructEvent_Seq);
               if B then
                  SeqLen := Length (MyStructEvent_Seq);
                  for Index in 1 .. SeqLen loop
                     Notification := Get_Element (MyStructEvent_Seq, Index);
                     Ada.Text_IO.Put_Line
                       ("Main components of structured event");
                     MyDomainName :=
                     Notification.header.fixed_header.event_type.domain_name;
                     Ada.Text_IO.Put ("My Domain : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
                     Ada.Text_IO.New_Line;

                     MyTypeName :=
                     Notification.header.fixed_header.event_type.type_name;
                     Ada.Text_IO.Put ("My Type : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
                     Ada.Text_IO.New_Line;

                     MyName :=  Notification.header.fixed_header.event_name;
                     Ada.Text_IO.Put ("My Name : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
                     Ada.Text_IO.New_Line;

                     MyBody :=  Notification.remainder_of_body;
                     Ada.Text_IO.Put ("Event Body : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String
                                     (CORBA.From_Any (MyBody)));
                     Ada.Text_IO.New_Line;
                  end loop;
               else
                  Ada.Text_IO.Put_Line ("Nothing to consume!!!");
               end if;
            end;

         when K_SequencePullConsumer =>
            declare
               C            : CosNotifyComm.SequencePullConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
               MyStructEvent_Seq : CosNotification.EventBatch;
               SeqLen       : Integer;
               Num_Event    : constant CORBA.Long := 2;
            begin
               C := CosNotifyComm.SequencePullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               CosNotifyComm.SequencePullConsumer.Impl.Try_Pull
                 (SequencePullConsumer.Impl.Object_Ptr (O),
                  Num_Event, B, MyStructEvent_Seq);
               if B then
                  SeqLen := Length (MyStructEvent_Seq);
                  for Index in 1 .. SeqLen loop
                     Notification := Get_Element (MyStructEvent_Seq, Index);
                     Ada.Text_IO.Put_Line
                       ("Main components of structured event");
                     MyDomainName :=
                     Notification.header.fixed_header.event_type.domain_name;
                     Ada.Text_IO.Put ("My Domain : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
                     Ada.Text_IO.New_Line;

                     MyTypeName :=
                     Notification.header.fixed_header.event_type.type_name;
                     Ada.Text_IO.Put ("My Type : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
                     Ada.Text_IO.New_Line;

                     MyName :=  Notification.header.fixed_header.event_name;
                     Ada.Text_IO.Put ("My Name : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
                     Ada.Text_IO.New_Line;

                     MyBody :=  Notification.remainder_of_body;
                     Ada.Text_IO.Put ("Event Body : ");
                     Ada.Text_IO.Put (CORBA.To_Standard_String
                                     (CORBA.From_Any (MyBody)));
                     Ada.Text_IO.New_Line;
                  end loop;
               else
                  Ada.Text_IO.Put_Line ("Nothing to consume!!!");
               end if;
            end;

         when K_StructuredPullConsumer =>
            declare
               C            : CosNotifyComm.StructuredPullConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
            begin
               C := CosNotifyComm.StructuredPullConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               CosNotifyComm.StructuredPullConsumer.Impl.Try_Pull
                 (StructuredPullConsumer.Impl.Object_Ptr (O), B, Notification);
               if B then
                  Ada.Text_IO.Put_Line ("Main components of structured event");
                  MyDomainName :=
                  Notification.header.fixed_header.event_type.domain_name;
                  Ada.Text_IO.Put ("My Domain : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
                  Ada.Text_IO.New_Line;

                  MyTypeName :=
                  Notification.header.fixed_header.event_type.type_name;
                  Ada.Text_IO.Put ("My Type : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
                  Ada.Text_IO.New_Line;

                  MyName :=  Notification.header.fixed_header.event_name;
                  Ada.Text_IO.Put ("My Name : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
                  Ada.Text_IO.New_Line;

                  MyBody :=  Notification.remainder_of_body;
                  Ada.Text_IO.Put ("Event Body : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String
                                  (CORBA.From_Any (MyBody)));
                  Ada.Text_IO.New_Line;
               else
                  Ada.Text_IO.Put_Line ("Nothing to consume!!!");
               end if;
            end;

         when K_StructuredPushConsumer =>
            declare
               C            : CosNotifyComm.StructuredPushConsumer.Ref;
               Notification : CosNotification.StructuredEvent;
               MyDomainName : CORBA.String;
               MyTypeName   : CORBA.String;
               MyName       : CORBA.String;
               MyBody       : CORBA.Any;
            begin
               C := CosNotifyComm.StructuredPushConsumer.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               CosNotifyComm.StructuredPushConsumer.Impl.Try_Pull
                 (StructuredPushConsumer.Impl.Object_Ptr (O), B, Notification);
               if B then
                  Ada.Text_IO.Put_Line ("Main components of structured event");
                  MyDomainName :=
                  Notification.header.fixed_header.event_type.domain_name;
                  Ada.Text_IO.Put ("My Domain : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyDomainName));
                  Ada.Text_IO.New_Line;

                  MyTypeName :=
                  Notification.header.fixed_header.event_type.type_name;
                  Ada.Text_IO.Put ("My Type : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyTypeName));
                  Ada.Text_IO.New_Line;

                  MyName :=  Notification.header.fixed_header.event_name;
                  Ada.Text_IO.Put ("My Name : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String (MyName));
                  Ada.Text_IO.New_Line;

                  MyBody :=  Notification.remainder_of_body;
                  Ada.Text_IO.Put ("Event Body : ");
                  Ada.Text_IO.Put (CORBA.To_Standard_String
                                  (CORBA.From_Any (MyBody)));
                  Ada.Text_IO.New_Line;
               else
                  Ada.Text_IO.Put_Line ("Nothing to consume!!!");
               end if;
            end;

         when others =>
            null;
      end case;
   end Try_Consume_Event;

   -------------------
   -- Create_Entity --
   -------------------

   procedure Create_Entity
     (Entity : out CORBA.Object.Ref;
      Kind   : in  Entity_Kind);

   procedure Create_Entity
     (Entity : out CORBA.Object.Ref;
      Kind   : in  Entity_Kind) is
   begin
      case Kind is
         when K_Channel =>
            declare
               F             : CosNotifyChannelAdmin.EventChannelFactory.Ref;
               R             : CosNotifyChannelAdmin.EventChannel.Ref;
               Id            : CosNotifyChannelAdmin.ChannelID;
               Initial_Admin : CosNotification.AdminProperties;
               Initial_QoS   : CosNotification.QoSProperties;
               MyProp        : CosNotification.Property;
               MyPropName    : CORBA.String;
            begin
               Servant_To_Reference
               (Servant (EventChannelFactory.Impl.Create), F);

               MyPropName := To_CORBA_String ("MaxConsumers");
               MyProp     := (CosNotification.PropertyName (MyPropName),
                              To_Any (CORBA.Long (10)));
               Append (Initial_Admin, MyProp);

               MyPropName := To_CORBA_String ("MaxSuppliers");
               MyProp     := (CosNotification.PropertyName (MyPropName),
                              To_Any (CORBA.Long (10)));
               Append (Initial_Admin, MyProp);

               EventChannelFactory.create_channel
               (F, Initial_QoS, Initial_Admin, Id, R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PullConsumer =>
            declare
               R : CosNotifyComm.PullConsumer.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.PullConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PullSupplier =>
            declare
               R : CosNotifyComm.PullSupplier.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.PullSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PushConsumer =>
            declare
               R : CosNotifyComm.PushConsumer.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.PushConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PushSupplier =>
            declare
               R : CosNotifyComm.PushSupplier.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.PushSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_SequencePullConsumer =>
            declare
               R : CosNotifyComm.SequencePullConsumer.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.SequencePullConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_SequencePullSupplier =>
            declare
               R : CosNotifyComm.SequencePullSupplier.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.SequencePullSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_SequencePushConsumer =>
            declare
               R : CosNotifyComm.SequencePushConsumer.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.SequencePushConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_SequencePushSupplier =>
            declare
               R : CosNotifyComm.SequencePushSupplier.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.SequencePushSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_StructuredPullConsumer =>
            declare
               R : CosNotifyComm.StructuredPullConsumer.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.StructuredPullConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_StructuredPullSupplier =>
            declare
               R : CosNotifyComm.StructuredPullSupplier.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.StructuredPullSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_StructuredPushConsumer =>
            declare
               R : CosNotifyComm.StructuredPushConsumer.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.StructuredPushConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_StructuredPushSupplier =>
            declare
               R : CosNotifyComm.StructuredPushSupplier.Ref;
            begin
               Servant_To_Reference
               (Servant (CosNotifyComm.StructuredPushSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;
      end case;
   end Create_Entity;

   -----------------
   -- Find_Entity --
   -----------------

   procedure Find_Entity
     (Name   : in  String_Access;
      Entity : out CORBA.Object.Ref;
      Kind   : out Entity_Kind);

   procedure Find_Entity
     (Name   : in  String_Access;
      Entity : out CORBA.Object.Ref;
      Kind   : out Entity_Kind)
   is
      Iter : BindingIterator.Ref;
      B    : Binding;
      BL   : BindingList;
      BI   : BindingIterator_Forward.Ref;
      Id   : CosNaming.Istring := CosNaming.To_CORBA_String (Name.all);
      Done : CORBA.Boolean;
      NC   : NameComponent;

   begin
      NamingContext.list (Ctx, 0, BL, BI);
      Iter := BindingIterator.Convert_Forward.To_Ref (BI);
      loop
         BindingIterator.next_one (Iter, B, Done);
         exit when not Done;
         NC := Get_Element (B.binding_name, 1);
         if NC.id = Id then
            for K in Image'Range loop
               if NC.kind = Image (K) then
                  Kind   := K;
                  Entity := NamingContext.resolve (Ctx, B.binding_name);
                  return;
               end if;
            end loop;
            raise Syntax_Error;
         end if;
      end loop;
      raise NamingContext.NotFound;
   end Find_Entity;

   -------------------
   -- Produce_Event --
   -------------------

   procedure Produce_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind;
      Event  : String_Access;
      Times  : Natural);

   procedure Produce_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind;
      Event  : String_Access;
      Times  : Natural)
   is
      O : CORBA.Impl.Object_Ptr;
      A : CORBA.Any := CORBA.To_Any (To_CORBA_String (Event.all));
   begin
      case Kind is

         when K_PullSupplier =>
            declare
               S : CosNotifyComm.PullSupplier.Ref;
            begin
               S := CosNotifyComm.PullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));
               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  CosNotifyComm.PullSupplier.Impl.Push
                  (CosNotifyComm.PullSupplier.Impl.Object_Ptr (O), A);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when K_PushSupplier =>
            declare
               S  : CosNotifyComm.PushSupplier.Ref;
            begin
               S := CosNotifyComm.PushSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));
               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  CosNotifyComm.PushSupplier.Impl.Push
                  (CosNotifyComm.PushSupplier.Impl.Object_Ptr (O), A);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when K_SequencePullSupplier =>
            declare
               S : CosNotifyComm.SequencePullSupplier.Ref;
               MyEventHeader         : CosNotification.EventHeader;
               MyFixedEventHeader    : CosNotification.FixedEventHeader;
               MyOpt_HeaderFields    : CosNotification.OptionalHeaderFields;
               MyEventType           : CosNotification.EventType;

               MyFilterableEventBody : CosNotification.FilterableEventBody;
               MyRemainder_Of_Body   : constant CORBA.Any := A;

               MyStructuredEvent1    : CosNotification.StructuredEvent;
               MyStructuredEvent2    : CosNotification.StructuredEvent;
               MyStructEvent_Seq     : CosNotification.EventBatch;
            begin
               S := CosNotifyComm.SequencePullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));

               --  Fill the details of the first structured event and
               --  append it to the sequence
               MyEventType.domain_name := To_CORBA_String
                                          ("Telecommunications");
               MyEventType.type_name   := To_CORBA_String
                                          ("Communication_Alarm");

               MyFixedEventHeader.event_type  := MyEventType;
               MyFixedEventHeader.event_name  := To_CORBA_String
                                                 ("MobileEvent");

               MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

               MyStructuredEvent1.header := MyEventHeader;
               MyStructuredEvent1.filterable_data := MyFilterableEventBody;
               MyStructuredEvent1.remainder_of_body := MyRemainder_Of_Body;

               Append (MyStructEvent_Seq, MyStructuredEvent1);

               --  Fill the details of the second structured event and
               --  append it to the sequence
               MyEventType.domain_name := To_CORBA_String
                                          ("Automobile");
               MyEventType.type_name   := To_CORBA_String
                                          ("Process_Control");

               MyFixedEventHeader.event_type  := MyEventType;
               MyFixedEventHeader.event_name  := To_CORBA_String
                                                 ("InventoryEvent");

               MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

               MyStructuredEvent2.header := MyEventHeader;
               MyStructuredEvent2.filterable_data := MyFilterableEventBody;
               MyStructuredEvent2.remainder_of_body := MyRemainder_Of_Body;

               Append (MyStructEvent_Seq, MyStructuredEvent2);

               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  CosNotifyComm.SequencePullSupplier.Impl.Push
                  (CosNotifyComm.SequencePullSupplier.Impl.Object_Ptr (O),
                   MyStructEvent_Seq);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when K_SequencePushSupplier =>
            declare
               S : CosNotifyComm.SequencePushSupplier.Ref;
               MyEventHeader         : CosNotification.EventHeader;
               MyFixedEventHeader    : CosNotification.FixedEventHeader;
               MyOpt_HeaderFields    : CosNotification.OptionalHeaderFields;
               MyEventType           : CosNotification.EventType;

               MyFilterableEventBody : CosNotification.FilterableEventBody;
               MyRemainder_Of_Body   : constant CORBA.Any := A;

               MyStructuredEvent1    : CosNotification.StructuredEvent;
               MyStructuredEvent2    : CosNotification.StructuredEvent;
               MyStructEvent_Seq     : CosNotification.EventBatch;
            begin
               S := CosNotifyComm.SequencePushSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));

               --  Fill the details of the first structured event and
               --  append it to the sequence
               MyEventType.domain_name := To_CORBA_String
                                          ("Telecommunications");
               MyEventType.type_name   := To_CORBA_String
                                          ("Communication_Alarm");

               MyFixedEventHeader.event_type  := MyEventType;
               MyFixedEventHeader.event_name  := To_CORBA_String
                                                 ("MobileEvent");

               MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

               MyStructuredEvent1.header := MyEventHeader;
               MyStructuredEvent1.filterable_data := MyFilterableEventBody;
               MyStructuredEvent1.remainder_of_body := MyRemainder_Of_Body;

               Append (MyStructEvent_Seq, MyStructuredEvent1);

               --  Fill the details of the second structured event and
               --  append it to the sequence
               MyEventType.domain_name := To_CORBA_String
                                          ("Automobile");
               MyEventType.type_name   := To_CORBA_String
                                          ("Process_Control");

               MyFixedEventHeader.event_type  := MyEventType;
               MyFixedEventHeader.event_name  := To_CORBA_String
                                                 ("InventoryEvent");

               MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

               MyStructuredEvent2.header := MyEventHeader;
               MyStructuredEvent2.filterable_data := MyFilterableEventBody;
               MyStructuredEvent2.remainder_of_body := MyRemainder_Of_Body;

               Append (MyStructEvent_Seq, MyStructuredEvent2);

               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  CosNotifyComm.SequencePushSupplier.Impl.Push
                  (CosNotifyComm.SequencePushSupplier.Impl.Object_Ptr (O),
                   MyStructEvent_Seq);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when K_StructuredPullSupplier =>
            declare
               S : CosNotifyComm.StructuredPullSupplier.Ref;
               MyEventHeader         : CosNotification.EventHeader;
               MyFixedEventHeader    : CosNotification.FixedEventHeader;
               MyOpt_HeaderFields    : CosNotification.OptionalHeaderFields;
               MyEventType           : CosNotification.EventType;

               MyFilterableEventBody : CosNotification.FilterableEventBody;
               MyRemainder_Of_Body   : constant CORBA.Any := A;

               MyStructuredEvent     : CosNotification.StructuredEvent;
            begin
               S := CosNotifyComm.StructuredPullSupplier.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));

               MyEventType.domain_name := To_CORBA_String
                                          ("Avionics");
               MyEventType.type_name   := To_CORBA_String
                                          ("Engine_Alarm");

               MyFixedEventHeader.event_type  := MyEventType;
               MyFixedEventHeader.event_name  := To_CORBA_String
                                                 ("TempratureEvent");

               MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

               MyStructuredEvent.header := MyEventHeader;
               MyStructuredEvent.filterable_data := MyFilterableEventBody;
               MyStructuredEvent.remainder_of_body := MyRemainder_Of_Body;

               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  CosNotifyComm.StructuredPullSupplier.Impl.Push
                  (CosNotifyComm.StructuredPullSupplier.Impl.Object_Ptr (O),
                   MyStructuredEvent);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when K_StructuredPushSupplier =>
            declare
               S : CosNotifyComm.StructuredPushSupplier.Ref;
               MyEventHeader         : CosNotification.EventHeader;
               MyFixedEventHeader    : CosNotification.FixedEventHeader;
               MyOpt_HeaderFields    : CosNotification.OptionalHeaderFields;
               MyEventType           : CosNotification.EventType;

               MyFilterableEventBody : CosNotification.FilterableEventBody;
               MyRemainder_Of_Body   : constant CORBA.Any := A;

               MyStructuredEvent     : CosNotification.StructuredEvent;
            begin
               S := CosNotifyComm.StructuredPushSupplier.
                    Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));

               MyEventType.domain_name := To_CORBA_String
                                          ("Telecommunications");
               MyEventType.type_name   := To_CORBA_String
                                          ("Communication_Alarm");

               MyFixedEventHeader.event_type  := MyEventType;
               MyFixedEventHeader.event_name  := To_CORBA_String
                                                 ("MobileEvent");

               MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

               MyStructuredEvent.header := MyEventHeader;
               MyStructuredEvent.filterable_data := MyFilterableEventBody;
               MyStructuredEvent.remainder_of_body := MyRemainder_Of_Body;

               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  CosNotifyComm.StructuredPushSupplier.Impl.Push
                  (CosNotifyComm.StructuredPushSupplier.Impl.Object_Ptr (O),
                   MyStructuredEvent);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when others =>
            null;
      end case;
   end Produce_Event;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (S   : String_Access;
      K   : Entity_Kind)
     return Name;

   function To_Name
     (S   : String_Access;
      K   : Entity_Kind)
      return Name
   is
      Element : NameComponent;
      Result  : Name;

   begin
      Element.id   := CosNaming.To_CORBA_String (S.all);
      Element.kind := CosNaming.To_CORBA_String (K'Img);
      Append (Result, Element);
      return Result;
   end To_Name;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help;

   procedure Display_Help is
   begin
      for C in Help_Messages'Range loop
         Ada.Text_IO.Put_Line
           (C'Img & ASCII.HT & ASCII.HT & Help_Messages (C).all);

         if C = Create then
            Ada.Text_IO.Put (ASCII.HT & "<kind> in");
            for E in Entity_Kind'Range loop
               declare
                  I : constant String := Entity_Kind'Image (E);
               begin
                  Ada.Text_IO.Put (' ' & I (3 .. I'Last));
               end;
            end loop;
            Ada.Text_IO.New_Line;
         end if;

      end loop;
      Ada.Text_IO.New_Line;
   end Display_Help;

   --------------
   -- Exit_All --
   --------------

   procedure Exit_All;

   procedure Exit_All is
   begin
      CORBA.ORB.Shutdown (False);
   end Exit_All;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop;

   procedure Main_Loop
   is
      Argc    : Natural;
      Entity  : CORBA.Object.Ref;
      Channel : CosNotifyChannelAdmin.EventChannel.Ref;
      Kind    : Entity_Kind;
   begin
      loop
         Argc := Count;
         if Argc > 0
           and then Argument (1)(Argument (1)'First) /= '#'
         then
            begin
               case Command'Value (Argument (1).all) is
                  when Help =>
                     Display_Help;

                  when Quit =>
                     Exit_All;
                     exit;

                  when Create =>
                     if Argc /= 3 then
                        raise Syntax_Error;
                     end if;

                     Kind := Entity_Kind'Value ("K_" & Argument (2).all);
                     declare
                        EK : Entity_Kind;

                     begin
                        Find_Entity (Argument (3), Entity, EK);

                        if EK /= Kind then
                           Ada.Text_IO.Put_Line
                             ("entity " & Argument (3).all &
                              " is a " & EK'Img);
                           raise Syntax_Error;
                        end if;

                     exception
                        when NamingContext.NotFound =>
                           Create_Entity (Entity, Kind);
                           NamingContext.bind
                             (Ctx, To_Name (Argument (3), Kind), Entity);
                     end;

                  when Connect =>
                     if Argc /= 4
                       or else Argument (3).all /= "to"
                     then
                        raise Syntax_Error;
                     end if;

                     Find_Entity (Argument (4), Entity, Kind);
                     if Kind /= K_Channel then
                        raise Syntax_Error;
                     end if;
                     Channel := EventChannel.Helper.To_Ref (Entity);

                     Find_Entity (Argument (2), Entity, Kind);
                     Connect_Entity (Entity, Kind, Channel);

                  when Consume =>
                     if Argc /= 3
                       or else Argument (2).all /= "in"
                     then
                        raise Syntax_Error;
                     end if;

                     Find_Entity (Argument (3), Entity, Kind);
                     Consume_Event (Entity, Kind);

                  when TryConsume =>
                     if Argc /= 3
                       or else Argument (2).all /= "in"
                     then
                        raise Syntax_Error;
                     end if;

                     Find_Entity (Argument (3), Entity, Kind);
                     Try_Consume_Event (Entity, Kind);

                  when Produce =>
                     if (Argc /= 4
                         and then Argc /= 6)
                       or else Argument (3).all /= "in"
                     then
                        raise Syntax_Error;
                     end if;

                     declare
                        N : Natural := 1;
                     begin
                        if Argc = 6 then
                           if Argument (6).all = "times" then
                              N  := Natural'Value (Argument (5).all);
                           else
                              raise Syntax_Error;
                           end if;
                        end if;

                        Find_Entity (Argument (4), Entity, Kind);
                        Produce_Event (Entity, Kind, Argument (2), N);
                     end;

                  when Run =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;

                     Set_Input (Argument (2));

                  when Sleep =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;

                     declare
                        N : constant Natural
                          := Natural'Value (Argument (2).all);
                     begin
                        delay Duration (N);
                     end;

                  when AutoDisplay =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;

                     declare
                        Item : String (1 .. 255);
                        Last : Natural;
                        C : PushConsumer.Ref;
                        O : CORBA.Impl.Object_Ptr;

                     begin
                        Find_Entity (Argument (2), Entity, Kind);
                        if Kind /= K_PushConsumer then
                           Ada.Text_IO.Put_Line
                             ("Can be called only with a PushConsumer");
                        else
                           C := PushConsumer.Helper.To_Ref (Entity);
                           Reference_To_Servant (C, Servant (O));
                           Ensure_Initialization;
                           Enter (Session_Mutex);
                           A_S := O;
                           Create_Task (Auto_Display'Access);
                           Wait (Session_Taken, Session_Mutex);
                           Leave (Session_Mutex);

                           Ada.Text_IO.Get_Line (Item, Last);
                           EndDisplay := True;
                        end if;
                     end;
               end case;

            exception
               when Syntax_Error =>
                  Ada.Text_IO.Put_Line ("syntax error");

               when E : others =>
                  Ada.Text_IO.Put_Line ("raise "& Exception_Name (E));
                  Ada.Text_IO.Put_Line (Exception_Message (E));
                  Ada.Text_IO.Put_Line (Exception_Information (E));
            end;
         end if;
      end loop;
   end Main_Loop;

   --  main procedure begins here.

begin

   CORBA.ORB.Initialize ("ORB");

   Initiate_Server (True);

   if Ada.Command_Line.Argument_Count = 0 then

      --  Test_Notification is used in interactive mode.

      if Count ("enter naming IOR [otherwise create one]: ") = 0 then
         Servant_To_Reference (Servant (NamingContext.Impl.Create), Ctx);
         Ada.Text_IO.Put_Line
           (CORBA.To_Standard_String
            (CORBA.Object.Object_To_String
             (CORBA.Object.Ref (Ctx))));

      else
         declare
            Obj : CORBA.Object.Ref;

         begin
            CORBA.ORB.String_To_Object
              (CORBA.To_CORBA_String (Argument (1).all), Obj);
            Ctx := NamingContext.Helper.To_Ref (Obj);
         end;
      end if;

      Display_Help;
      Main_Loop;

   else

      --  Test_Notification is used in batch mode.

      if Ada.Command_Line.Argument_Count /= 2 then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("usage: test_notification [<COSNAMING_IOR> <script_file>]");
         Exit_All;
      end if;

      declare
         Obj : CORBA.Object.Ref;
      begin
         CORBA.ORB.String_To_Object
           (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Obj);
         Ctx := NamingContext.Helper.To_Ref (Obj);
      end;

      Set_Input (+Ada.Command_Line.Argument (2));

      Main_Loop;
   end if;
end Test_Notification;
