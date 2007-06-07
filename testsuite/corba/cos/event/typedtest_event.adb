------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T Y P E D T E S T _ E V E N T                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
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

--  Test PolyORB COS Typed event capabilities.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPullConsumer.Impl;

with CosEventChannelAdmin.ProxyPushSupplier;
with CosEventChannelAdmin.ProxyPushSupplier.Impl;

with CosEventComm.PullConsumer.Helper;
with CosEventComm.PullConsumer.Impl;

with CosEventComm.PullSupplier.Helper;

with CosEventComm.PushConsumer.Helper;

with CosEventComm.PushSupplier.Helper;
with CosEventComm.PushSupplier.Impl;

with CosNaming.BindingIterator;

with CosNaming.NamingContext.Impl;
with CosNaming.NamingContext.Helper;

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedConsumerAdmin;
with CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl;

with CosTypedEventChannelAdmin.TypedEventChannel.Impl;
with CosTypedEventChannelAdmin.TypedEventChannel.Helper;

with CosTypedEventChannelAdmin.TypedProxyPullSupplier;
with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl;

with CosTypedEventChannelAdmin.TypedProxyPushConsumer;
with CosTypedEventChannelAdmin.TypedProxyPushConsumer.Impl;

with CosTypedEventChannelAdmin.TypedSupplierAdmin;
with CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl;

with CosTypedEventComm.TypedPullSupplier.Helper;
with CosTypedEventComm.TypedPullSupplier.Impl;

with CosTypedEventComm.TypedPushConsumer.Helper;
with CosTypedEventComm.TypedPushConsumer.Impl;

with Menu;
--  From COS Naming, provide text interface.

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with GNAT.OS_Lib;

with TypedTest_Interface;
with TypedTest_Interface.Helper;
with TypedTest_Interface.Impl;
--  Mutually Agreed Interface between Consumers and Suppliers

procedure TypedTest_Event is

   use Ada.Exceptions;

   use CORBA;

   use Menu;

   use CosEventChannelAdmin;

   use CosEventComm;
   use CosEventComm.PullConsumer.Impl;
   use CosEventComm.PushSupplier.Impl;

   use CosNaming;

   use CosTypedEventChannelAdmin;
   use CosTypedEventChannelAdmin.TypedEventChannel.Impl;

   use CosTypedEventComm;
   use CosTypedEventComm.TypedPushConsumer.Impl;
   use CosTypedEventComm.TypedPullSupplier.Impl;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;

   type Command is
     (Help,
      Quit,
      Run,
      Sleep,
      Get_Typed_Object,
      Connect,
      Create);

   Syntax_Error : exception;

   Help_Messages : constant array (Command) of String_Access
     := (Help        => +(ASCII.HT & "print this message"),
         Quit        => +(ASCII.HT & "quit this shell"),
         Run         => +(ASCII.HT
                          & "run <file of commands from this language>"),
         Sleep       => +(ASCII.HT & "sleep <seconds>"),
         Create      => +(ASCII.HT & "create <kind> <entity>"),
         Get_Typed_Object
                     => +(ASCII.HT & "get_typed_object on " &
                                    "<entity> from <channel>"),
         Connect     => +(ASCII.HT & "connect <entity> to <channel>"));

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
     (Entity  : in CORBA.Object.Ref;
      Kind    : in Entity_Kind;
      Channel : in TypedEventChannel.Ref);

   procedure Connect_Entity
     (Entity  : in CORBA.Object.Ref;
      Kind    : in Entity_Kind;
      Channel : in TypedEventChannel.Ref)
   is
      O : CORBA.Impl.Object_Ptr;
   begin
      case Kind is
         when K_PullSupplier =>
            declare
               A   : TypedSupplierAdmin.Ref;
               P   : ProxyPullConsumer.Ref;
               E   : TypedPullSupplier.Ref;
               NE  : PullSupplier.Ref;
               Create_Ptr : TypedEventChannel.Impl.Interface_Ptr;
               RID : CORBA.String;
            begin
               E := TypedPullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               Create_Ptr := TypedTest_Interface.Impl.Create'Access;
               RID := CORBA.To_CORBA_String
                      (Standard.String'(TypedTest_Interface.Repository_Id));

               TypedPullSupplier.Impl.SetInterface_Ptr
               (TypedPullSupplier.Impl.Object_Ptr (O), Create_Ptr);

               A := TypedEventChannel.for_suppliers (Channel);

               Reference_To_Servant (A, Servant (O));

               P := TypedSupplierAdmin.Impl.obtain_typed_pull_consumer
                   (TypedSupplierAdmin.Impl.Object_Ptr (O),
                    CosTypedEventChannelAdmin.Key (RID));

               Reference_To_Servant (P, Servant (O));
               NE := PullSupplier.Helper.To_Ref (Entity);
               ProxyPullConsumer.Impl.Connect_Pull_Supplier
               (ProxyPullConsumer.Impl.Object_Ptr (O), NE);
            end;

         when K_PushConsumer =>
            declare
               A  : TypedConsumerAdmin.Ref;
               P  : ProxyPushSupplier.Ref;
               E  : TypedPushConsumer.Ref;
               NE : PushConsumer.Ref;
               Create_Ptr : TypedEventChannel.Impl.Interface_Ptr;
               RID : CORBA.String;
            begin
               E := TypedPushConsumer.Helper.To_Ref (Entity);
               Reference_To_Servant (E, Servant (O));
               Create_Ptr := TypedTest_Interface.Impl.Create'Access;
               RID := CORBA.To_CORBA_String
                      (Standard.String'(TypedTest_Interface.Repository_Id));

               TypedPushConsumer.Impl.SetInterface_Ptr
                 (TypedPushConsumer.Impl.Object_Ptr (O), Create_Ptr);

               A := TypedEventChannel.for_consumers (Channel);
               Reference_To_Servant (A, Servant (O));
               P := TypedConsumerAdmin.Impl.obtain_typed_push_supplier
                    (TypedConsumerAdmin.Impl.Object_Ptr (O),
                     CosTypedEventChannelAdmin.Key (RID));

               Reference_To_Servant (P, Servant (O));
               NE := PushConsumer.Helper.To_Ref (Entity);
               ProxyPushSupplier.Impl.Connect_Push_Consumer
               (ProxyPushSupplier.Impl.Object_Ptr (O), NE);
            end;

         when others =>
            raise Syntax_Error;
      end case;
   end Connect_Entity;

   -----------------------------
   -- Get_Typed_Object_Entity --
   -----------------------------

   procedure Get_Typed_Object_Entity
     (Entity  : in CORBA.Object.Ref;
      Kind    : in Entity_Kind;
      Channel : in TypedEventChannel.Ref);

   procedure Get_Typed_Object_Entity
     (Entity  : in CORBA.Object.Ref;
      Kind    : in Entity_Kind;
      Channel : in TypedEventChannel.Ref)
   is
      O : CORBA.Impl.Object_Ptr;
   begin
      case Kind is
         when K_PullConsumer =>
            declare
               A   : TypedConsumerAdmin.Ref;
               P   : TypedProxyPullSupplier.Ref;
               E   : PullConsumer.Ref;
               RID : CORBA.String;
               T   : CORBA.Object.Ref;
               TI  : TypedTest_Interface.Ref;
               SendMsg, RecMsg : CORBA.String;
            begin
               A := TypedEventChannel.for_consumers (Channel);
               RID := CORBA.To_CORBA_String
                      (Standard.String'(TypedTest_Interface.Repository_Id));

               Reference_To_Servant (A, Servant (O));
               P := TypedConsumerAdmin.Impl.obtain_typed_pull_supplier
                    (TypedConsumerAdmin.Impl.Object_Ptr (O),
                     CosTypedEventChannelAdmin.Key (RID));

               E := PullConsumer.Helper.To_Ref (Entity);

               Reference_To_Servant (P, Servant (O));
               TypedProxyPullSupplier.Impl.Connect_Pull_Consumer
                 (TypedProxyPullSupplier.Impl.Object_Ptr (O), E);

               T := TypedProxyPullSupplier.Impl.Get_Typed_Supplier
                    (TypedProxyPullSupplier.Impl.Object_Ptr (O));

               TI := TypedTest_Interface.Helper.To_Ref (T);
               Ada.Text_IO.Put_Line ("Calling operations defined in Mutually" &
                                     " Agreed Interface");

               Reference_To_Servant (TI, Servant (O));
               SendMsg := CORBA.To_CORBA_String
                      (Standard.String'("Hello to TestInterface"));
               RecMsg := TypedTest_Interface.Impl.EchoString
                         (TypedTest_Interface.Impl.Object_Ptr (O), SendMsg);
               Ada.Text_IO.Put_Line ("Msg from Test Interface : "&
                               CORBA.To_Standard_String (RecMsg));
            end;
         when K_PushSupplier =>
            declare
               A   : TypedSupplierAdmin.Ref;
               P   : TypedProxyPushConsumer.Ref;
               E   : PushSupplier.Ref;
               RID : CORBA.String;
               T   : CORBA.Object.Ref;
               TI  : TypedTest_Interface.Ref;
               SendMsg, RecMsg : CORBA.String;
            begin
               A := TypedEventChannel.for_suppliers (Channel);
               RID := CORBA.To_CORBA_String
                      (Standard.String'(TypedTest_Interface.Repository_Id));

               Reference_To_Servant (A, Servant (O));
               P := TypedSupplierAdmin.Impl.obtain_typed_push_consumer
                    (TypedSupplierAdmin.Impl.Object_Ptr (O),
                     CosTypedEventChannelAdmin.Key (RID));

               E := PushSupplier.Helper.To_Ref (Entity);

               Reference_To_Servant (P, Servant (O));
               TypedProxyPushConsumer.Impl.Connect_Push_Supplier
                 (TypedProxyPushConsumer.Impl.Object_Ptr (O), E);

               T := TypedProxyPushConsumer.Impl.Get_Typed_Consumer
                    (TypedProxyPushConsumer.Impl.Object_Ptr (O));

               TI := TypedTest_Interface.Helper.To_Ref (T);
               Ada.Text_IO.Put_Line ("Calling operations defined in Mutually" &
                                     " Agreed Interface");
               Reference_To_Servant (TI, Servant (O));
               SendMsg := CORBA.To_CORBA_String
                      (Standard.String'("Hello to TestInterface"));
               RecMsg := TypedTest_Interface.Impl.EchoString
                         (TypedTest_Interface.Impl.Object_Ptr (O), SendMsg);
               Ada.Text_IO.Put_Line ("Msg from Test Interface : "&
                               CORBA.To_Standard_String (RecMsg));
            end;

         when others =>
            raise Syntax_Error;
      end case;
   end Get_Typed_Object_Entity;

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
               R : TypedEventChannel.Ref;

            begin
               Servant_To_Reference
               (Servant (TypedEventChannel.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PullConsumer =>
            declare
               R : PullConsumer.Ref;

            begin
               Servant_To_Reference
               (Servant (PullConsumer.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PushSupplier =>
            declare
               R : PushSupplier.Ref;

            begin
               Servant_To_Reference (Servant (PushSupplier.Impl.Create), R);
               Entity := CORBA.Object.Ref (R);
            end;

         when K_PullSupplier =>
            declare
               R          : TypedPullSupplier.Ref;
               RID        : CORBA.String;
               Create_Ptr : TypedEventChannel.Impl.Interface_Ptr;
            begin
               Servant_To_Reference
               (Servant (TypedPullSupplier.Impl.Create), R);

               Create_Ptr := TypedTest_Interface.Impl.Create'Access;
               RID := CORBA.To_CORBA_String
                      (Standard.String'(TypedTest_Interface.Repository_Id));
               TypedEventChannel.Impl.Register
               (CosTypedEventChannelAdmin.Key (RID), Create_Ptr);

               Entity := CORBA.Object.Ref (R);
            end;

         when K_PushConsumer =>
            declare
               R : TypedPushConsumer.Ref;
               RID        : CORBA.String;
               Create_Ptr : TypedEventChannel.Impl.Interface_Ptr;
            begin
               Servant_To_Reference
               (Servant (TypedPushConsumer.Impl.Create), R);

               Create_Ptr := TypedTest_Interface.Impl.Create'Access;
               RID := CORBA.To_CORBA_String
                      (Standard.String'(TypedTest_Interface.Repository_Id));
               TypedEventChannel.Impl.Register
               (CosTypedEventChannelAdmin.Key (RID), Create_Ptr);

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
      GNAT.OS_Lib.OS_Exit (1);
   end Exit_All;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop;

   procedure Main_Loop
   is
      Argc    : Natural;
      Entity  : CORBA.Object.Ref;
      Channel : TypedEventChannel.Ref;
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
                     Channel := TypedEventChannel.Helper.To_Ref (Entity);

                     Find_Entity (Argument (2), Entity, Kind);
                     Connect_Entity (Entity, Kind, Channel);

                  when Get_Typed_Object =>
                     if Argc /= 5
                       or else Argument (2).all /= "on"
                       or else Argument (4).all /= "from"
                     then
                        raise Syntax_Error;
                     end if;

                     Find_Entity (Argument (5), Entity, Kind);
                     if Kind /= K_Channel then
                        raise Syntax_Error;
                     end if;
                     Channel := TypedEventChannel.Helper.To_Ref (Entity);

                     Find_Entity (Argument (3), Entity, Kind);
                     Get_Typed_Object_Entity (Entity, Kind, Channel);

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
end TypedTest_Event;
