------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                           T E S T _ E V E N T                            --
--                                                                          --
--                                 B o d y                                  --
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

with CosNaming; use CosNaming;

with CosNaming.NamingContext;
with CosNaming.NamingContext.Impl;
with CosNaming.NamingContext.Helper;

with CosNaming.BindingIterator;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.EventChannel;
with CosEventChannelAdmin.EventChannel.Impl;
with CosEventChannelAdmin.EventChannel.Helper;
with CosEventChannelAdmin.ConsumerAdmin;
with CosEventChannelAdmin.SupplierAdmin;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPullSupplier;
with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.ProxyPushSupplier;

with CosEventComm; use CosEventComm;

with CosEventComm.PullConsumer;
with CosEventComm.PullSupplier;
with CosEventComm.PushConsumer;
with CosEventComm.PushSupplier;

with CosEventComm.PullConsumer.Helper;
with CosEventComm.PullSupplier.Helper;
with CosEventComm.PushConsumer.Helper;
with CosEventComm.PushSupplier.Helper;

with CosEventComm.PullConsumer.Impl;
with CosEventComm.PullSupplier.Impl;
with CosEventComm.PushConsumer.Impl;
with CosEventComm.PushSupplier.Impl;

with CORBA;
with CORBA.Object;
with CORBA.Impl;
with CORBA.ORB;

with Broca.Server_Tools; use Broca.Server_Tools;

with PortableServer; use PortableServer;

with Menu; use Menu;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Test_Event is

   type Command is
     (Help,
      Quit,
      Run,
      Sleep,
      Connect,
      Consume,
      Produce,
      Create);

   Syntax_Error : exception;

   function M (S : String) return String_Access;
   function M (S : String) return String_Access is
   begin
      return new String'(S);
   end M;

   Help_Messages : constant array (Command) of String_Access
     := (Help    => M ("print this message"),
         Quit    => M ("quit this shell"),
         Run     => M ("run <file of commands from this language"),
         Sleep   => M ("sleep <seconds>"),
         Create  => M ("create <kind> <entity>"),
         Connect => M ("connect <entity> to <channel>"),
         Consume => M ("consume in <entity>"),
         Produce => M ("produce <string> in <entity>"));

   type Entity_Kind is
     (K_Channel,
      K_PullConsumer,
      K_PullSupplier,
      K_PushConsumer,
      K_PushSupplier);

   Image : array (Entity_Kind) of CosNaming.Istring
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
      Channel : in EventChannel.Ref)
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
               A := EventChannel.For_Consumers (Channel);
               P := ConsumerAdmin.Obtain_Pull_Supplier (A);
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
               A := EventChannel.For_Suppliers (Channel);
               P := SupplierAdmin.Obtain_Pull_Consumer (A);
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
               A := EventChannel.For_Consumers (Channel);
               P := ConsumerAdmin.Obtain_Push_Supplier (A);
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
               A := EventChannel.For_Suppliers (Channel);
               P := SupplierAdmin.Obtain_Push_Consumer (A);
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

   -------------------
   -- Create_Entity --
   -------------------

   procedure Create_Entity
     (Entity : out CORBA.Object.Ref;
      Kind   : in Entity_Kind) is
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
     (Name   : in String_Access;
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
      NamingContext.List (Ctx, 0, BL, BI);
      Iter := BindingIterator.Convert_Forward.To_Ref (BI);
      loop
         BindingIterator.Next_One (Iter, B, Done);
         exit when not Done;
         NC := Element_Of (B.Binding_Name, 1);
         if NC.Id = Id then
            for K in Image'Range loop
               if NC.Kind = Image (K) then
                  Kind   := K;
                  Entity := NamingContext.Resolve (Ctx, B.Binding_Name);
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
      Event  : String_Access)
   is
      O : CORBA.Impl.Object_Ptr;
      A : CORBA.Any := To_Any (To_CORBA_String (Event.all));

   begin
      case Kind is
         when K_PullSupplier =>
            declare
               S : PullSupplier.Ref;

            begin
               S := PullSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));
               PullSupplier.Impl.Push (PullSupplier.Impl.Object_Ptr (O), A);
            end;

         when K_PushSupplier =>
            declare
               S : PushSupplier.Ref;

            begin
               S := PushSupplier.Helper.To_Ref (Entity);
               Reference_To_Servant (S, Servant (O));
               PushSupplier.Impl.Push (PushSupplier.Impl.Object_Ptr (O), A);
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
      return Name
   is
      Element : NameComponent;
      Result  : Name;

   begin
      Element.Id   := CosNaming.To_CORBA_String (S.all);
      Element.Kind := CosNaming.To_CORBA_String (K'Img);
      Append (Result, Element);
      return Result;
   end To_Name;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      for C in Help_Messages'Range loop
         Ada.Text_IO.Put_Line
           (C'Img & Ascii.HT & Help_Messages (C).all);
         if C = Create then
            Ada.Text_IO.Put (Ascii.HT & "<kind> in");
            for E in Entity_Kind'Range loop
               declare
                  I : String := E'Img;
               begin
                  Ada.Text_IO.Put (' ' & I (3 .. I'Last));
               end;
            end loop;
            Ada.Text_IO.New_Line;
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Usage;

   Argc    : Natural;
   Entity  : CORBA.Object.Ref;
   Channel : EventChannel.Ref;
   Kind    : Entity_Kind;

begin
   Initiate_Server;

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

   loop
      Argc := Count;
      if Argc > 0
        and then Argument (1)(Argument (1)'First) /= '#'
      then
         begin
            case Command'Value (Argument (1).all) is
               when Help =>
                  Usage;

               when Quit =>
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
                        NamingContext.Bind
                          (Ctx, To_Name (Argument (3), Kind), Entity);
                  end;

               when Connect =>
                  if Argc /= 4 then
                     raise Syntax_Error;
                  end if;

                  if Argument (3).all /= "to" then
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
                  if Argc /= 3 then
                     raise Syntax_Error;
                  end if;

                  if Argument (2).all /= "in" then
                     raise Syntax_Error;
                  end if;

                  Find_Entity (Argument (3), Entity, Kind);
                  Ada.Text_IO.Put_Line (Consume_Event (Entity, Kind));

               when Produce =>
                  if Argc /= 4 then
                     raise Syntax_Error;
                  end if;

                  if Argument (3).all /= "in" then
                     raise Syntax_Error;
                  end if;
                  Find_Entity (Argument (4), Entity, Kind);
                  Produce_Event (Entity, Kind, Argument (2));

               when Run =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;

                  Menu.Set_Input (Argument (2));

               when Sleep =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;

                  declare
                     N : Natural := Natural'Value (Argument (2).all);
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
         end;
      end if;
   end loop;

end Test_Event;
