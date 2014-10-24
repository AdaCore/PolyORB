------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           T E S T _ E V E N T                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2014, Free Software Foundation, Inc.          --
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

--  Test PolyORB COS event capabilities.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer;

with CosNaming.BindingIterator;

with CosNaming.NamingContext.Impl;
with CosNaming.NamingContext.Helper;

with CosEventChannelAdmin.ConsumerAdmin;

with CosEventChannelAdmin.EventChannel.Impl;
with CosEventChannelAdmin.EventChannel.Helper;

with CosEventChannelAdmin.SupplierAdmin;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPullSupplier;
with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.ProxyPushSupplier;

with CosEventComm.PullConsumer.Helper;
with CosEventComm.PullConsumer.Impl;

with CosEventComm.PullSupplier.Helper;
with CosEventComm.PullSupplier.Impl;

with CosEventComm.PushConsumer.Helper;
with CosEventComm.PushConsumer.Impl;

with CosEventComm.PushSupplier.Helper;
with CosEventComm.PushSupplier.Impl;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with Auto_Print;
--  Auxiliary code to output pushconsumer incoming messages.

with Menu;
--  From COS Naming, provide text interface.

procedure Test_Event is

   use Ada.Exceptions;

   use Auto_Print;
   use Menu;

   use PortableServer;

   use CosNaming;
   use CosEventChannelAdmin;
   use CosEventComm;
   use CosEventComm.PushConsumer.Impl;

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
      K_PushSupplier);

   Image : constant array (Entity_Kind) of CosNaming.Istring
     := (K_Channel      => CosNaming.To_CORBA_String (K_Channel'Img),
         K_PullConsumer => CosNaming.To_CORBA_String (K_PullConsumer'Img),
         K_PullSupplier => CosNaming.To_CORBA_String (K_PullSupplier'Img),
         K_PushConsumer => CosNaming.To_CORBA_String (K_PushConsumer'Img),
         K_PushSupplier => CosNaming.To_CORBA_String (K_PushSupplier'Img));

   Ctx : NamingContext.Ref;

   --------------------
   -- Connect_Entity --
   --------------------

   procedure Connect_Entity
     (Entity  : CORBA.Object.Ref;
      Kind    : Entity_Kind;
      Channel : EventChannel.Ref);

   procedure Connect_Entity
     (Entity  : CORBA.Object.Ref;
      Kind    : Entity_Kind;
      Channel : EventChannel.Ref)
   is
      O : CORBA.Impl.Object_Ptr;

   begin
      case Kind is
         when K_PullConsumer =>
            declare
               A : ConsumerAdmin.Ref;
               P : ProxyPullSupplier.Ref;
               E : PullConsumer.Ref;

            begin
               A := EventChannel.for_consumers (Channel);
               P := ConsumerAdmin.obtain_pull_supplier (A);
               E := PullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PullConsumer.Impl.Connect_Proxy_Pull_Supplier
                 (PullConsumer.Impl.Object_Ptr (O), P);
            end;

         when K_PullSupplier =>
            declare
               A : SupplierAdmin.Ref;
               P : ProxyPullConsumer.Ref;
               E : PullSupplier.Ref;

            begin
               A := EventChannel.for_suppliers (Channel);
               P := SupplierAdmin.obtain_pull_consumer (A);
               E := PullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PullSupplier.Impl.Connect_Proxy_Pull_Consumer
                 (PullSupplier.Impl.Object_Ptr (O), P);
            end;

         when K_PushConsumer =>
            declare
               A : ConsumerAdmin.Ref;
               P : ProxyPushSupplier.Ref;
               E : PushConsumer.Ref;

            begin
               A := EventChannel.for_consumers (Channel);
               P := ConsumerAdmin.obtain_push_supplier (A);
               E := PushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PushConsumer.Impl.Connect_Proxy_Push_Supplier
                 (PushConsumer.Impl.Object_Ptr (O), P);
            end;

         when K_PushSupplier =>
            declare
               A : SupplierAdmin.Ref;
               P : ProxyPushConsumer.Ref;
               E : PushSupplier.Ref;

            begin
               A := EventChannel.for_suppliers (Channel);
               P := SupplierAdmin.obtain_push_consumer (A);
               E := PushSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               PushSupplier.Impl.Connect_Proxy_Push_Consumer
                 (PushSupplier.Impl.Object_Ptr (O), P);
            end;

         when K_Channel =>
            raise Syntax_Error;
      end case;
   end Connect_Entity;

   -------------------
   -- Consume_Event --
   -------------------

   function Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind)
     return String;

   function Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind)
     return String
   is
      O : CORBA.Impl.Object_Ptr;
      A : CORBA.Any;

   begin
      case Kind is
         when K_PullConsumer =>
            declare
               C : PullConsumer.Ref;

            begin
               C := PullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               A := PullConsumer.Impl.Pull (PullConsumer.Impl.Object_Ptr (O));
               return To_Standard_String (From_Any (A));
            end;

         when K_PushConsumer =>
            declare
               C : PushConsumer.Ref;
            begin
               C := PushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               A := PushConsumer.Impl.Pull (PushConsumer.Impl.Object_Ptr (O));
               return To_Standard_String (From_Any (A));
            end;

         when others =>
            null;
      end case;
      return "";
   end Consume_Event;

   -----------------------
   -- Try_Consume_Event --
   -----------------------

   function Try_Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind)
     return String;

   function Try_Consume_Event
     (Entity : CORBA.Object.Ref;
      Kind   : Entity_Kind)
     return String
   is
      O : CORBA.Impl.Object_Ptr;
      A : CORBA.Any;
      B : CORBA.Boolean;
   begin
      case Kind is
         when K_PullConsumer =>
            declare
               C : PullConsumer.Ref;

            begin
               C := PullConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               PullConsumer.Impl.Try_Pull
                 (PullConsumer.Impl.Object_Ptr (O), B, A);
               if B then
                  return To_Standard_String (From_Any (A));
               else
                  return "Nothing to consume!!!";
               end if;
            end;

         when K_PushConsumer =>
            declare
               C : PushConsumer.Ref;
            begin
               C := PushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (C, Servant (O));
               PushConsumer.Impl.Try_Pull
                 (PushConsumer.Impl.Object_Ptr (O), B, A);
               if B then
                  return To_Standard_String (From_Any (A));
               else
                  return "Nothing to consume!!!";
               end if;
            end;

         when others =>
            null;
      end case;
      return "";
   end Try_Consume_Event;

   -------------------
   -- Create_Entity --
   -------------------

   procedure Create_Entity
     (Entity : out CORBA.Object.Ref;
      Kind   : Entity_Kind);

   procedure Create_Entity
     (Entity : out CORBA.Object.Ref;
      Kind   : Entity_Kind) is
   begin
      case Kind is
         when K_Channel =>
            declare
               R : EventChannel.Ref;

            begin
               Servant_To_Reference (Servant (EventChannel.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PullConsumer =>
            declare
               R : PullConsumer.Ref;

            begin
               Servant_To_Reference (Servant (PullConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PullSupplier =>
            declare
               R : PullSupplier.Ref;

            begin
               Servant_To_Reference (Servant (PullSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PushConsumer =>
            declare
               R : PushConsumer.Ref;

            begin
               Servant_To_Reference (Servant (PushConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PushSupplier =>
            declare
               R : PushSupplier.Ref;

            begin
               Servant_To_Reference (Servant (PushSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;
      end case;
   end Create_Entity;

   -----------------
   -- Find_Entity --
   -----------------

   procedure Find_Entity
     (Name   : String_Access;
      Entity : out CORBA.Object.Ref;
      Kind   : out Entity_Kind);

   procedure Find_Entity
     (Name   : String_Access;
      Entity : out CORBA.Object.Ref;
      Kind   : out Entity_Kind)
   is
      Iter : BindingIterator.Ref;
      B    : Binding;
      BL   : BindingList;
      BI   : BindingIterator_Forward.Ref;
      Id   : constant CosNaming.Istring
        := CosNaming.To_CORBA_String (Name.all);
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
      A : constant CORBA.Any := To_Any (To_CORBA_String (Event.all));

   begin
      case Kind is
         when K_PullSupplier =>
            declare
               S : PullSupplier.Ref;

            begin
               S := PullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));
               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  PullSupplier.Impl.Push (PullSupplier.Impl.Object_Ptr (O), A);
                  Ada.Text_IO.Put (".");
               end loop;
               Ada.Text_IO.New_Line;
            end;

         when K_PushSupplier =>
            declare
               S : PushSupplier.Ref;

            begin
               S := PushSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));
               Ada.Text_IO.New_Line;
               for J in 1 .. Times loop
                  PushSupplier.Impl.Push (PushSupplier.Impl.Object_Ptr (O), A);
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
      Channel : EventChannel.Ref;
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
                     Ada.Text_IO.Put_Line (Consume_Event (Entity, Kind));

                  when TryConsume =>
                     if Argc /= 3
                       or else Argument (2).all /= "in"
                     then
                        raise Syntax_Error;
                     end if;

                     Find_Entity (Argument (3), Entity, Kind);
                     Ada.Text_IO.Put_Line (Try_Consume_Event (Entity, Kind));

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
                           Create_Task (Auto_Display'Access, "Auto_Display");
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
                  Ada.Text_IO.Put_Line ("raise " & Exception_Name (E));
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

      --  Test_Event is used in interactive mode.

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

      --  Test_Event is used in batch mode.

      if Ada.Command_Line.Argument_Count /= 2 then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("usage: test_event [<COSNAMING_IOR> <script_file>]");
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
end Test_Event;
