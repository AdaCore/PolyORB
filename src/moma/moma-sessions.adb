------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . S E S S I O N S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with MOMA.Provider.Message_Consumer;
with MOMA.Provider.Message_Producer;
with MOMA.Provider.Message_Handler;

with PolyORB;
with PolyORB.Annotations;
with PolyORB.Call_Back;
with PolyORB.Minimal_Servant.Tools;
with PolyORB.References.IOR;
with PolyORB.Types;

package body MOMA.Sessions is

   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Destinations;

   use PolyORB.Minimal_Servant.Tools;

   ------------
   --  Close --
   ------------

   procedure Close is
   begin
      null;
      --  XXX Not Implemented
   end Close;

   -------------
   --  Commit --
   -------------

   procedure Commit is
   begin
      null;
      --  XXX Not Implemented
   end Commit;

   ------------------------
   -- Create_Destination --
   ------------------------

   function Create_Destination (Name   : MOMA.Types.String;
                                Remote : PolyORB.References.Ref)
                                return MOMA.Destinations.Destination
   is
   begin
      return MOMA.Destinations.Create (Name, Remote, MOMA.Types.Pool);
   end Create_Destination;

   ---------------------
   -- Create_Handler --
   ---------------------

   function Create_Handler
     (Self           : Session;
      Message_Cons   : MOMA.Message_Consumers.Message_Consumer_Acc)
      return MOMA.Message_Handlers.Message_Handler_Acc
   is
      Handler : MOMA.Message_Handlers.Message_Handler_Acc :=
         new MOMA.Message_Handlers.Message_Handler;
      Servant : constant MOMA.Provider.Message_Handler.Object_Acc :=
         new MOMA.Provider.Message_Handler.Object;
      Servant_Ref : PolyORB.References.Ref;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      --  XXX Self is to be used to 'place' the receiver
      --  using session position in the POA.
      --  XXX XXX (same as in Create_Receiver)
      Initiate_Servant (Servant,
                        MOMA.Provider.Message_Handler.If_Desc,
                        MOMA.Types.MOMA_Type_Id,
                        Servant_Ref);
      MOMA.Message_Handlers.Initialize (
         Handler, Message_Cons, Servant_Ref, null, null);
      return Handler;
   end Create_Handler;

   ---------------------
   -- Create_Receiver --
   ---------------------

   function Create_Receiver (Self : Session;
                             Dest : MOMA.Destinations.Destination)
      return MOMA.Message_Consumers.Message_Consumer_Acc
   is
      MOMA_Obj : constant MOMA.Provider.Message_Consumer.Object_Acc
        := new MOMA.Provider.Message_Consumer.Object;

      MOMA_Ref : PolyORB.References.Ref;

      Consumer : constant MOMA.Message_Consumers.Message_Consumer_Acc :=
         new MOMA.Message_Consumers.Message_Consumer;

   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      --  XXX self is to be used to 'place' the receiver
      --  using session position in the POA

      MOMA_Obj.Remote_Ref := Get_Ref (Dest);
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Consumer.If_Desc,
                        MOMA.Types.MOMA_Type_Id,
                        MOMA_Ref);

      Set_Destination (Consumer.all, Dest);
      Set_Ref (Consumer.all, MOMA_Ref);
      --  XXX Is it really useful to have the Ref to the remote destination in
      --  the Message_Consumer itself ? By construction, this ref is
      --  encapsulated in the MOMA.Provider.Message_Consumer.Object ....
      return Consumer;
   end Create_Receiver;

   ---------------------
   -- Create_Receiver --
   ---------------------

   function Create_Receiver (Dest             : MOMA.Destinations.Destination;
                             Message_Selector : MOMA.Types.String)
      return MOMA.Message_Consumers.Message_Consumer
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Receiver (Dest, Message_Selector);
      pragma Warnings (On);
   end Create_Receiver;

   -------------------
   -- Create_Sender --
   -------------------

   function Create_Sender (Self : Session;
                           Dest : MOMA.Destinations.Destination)
                           return MOMA.Message_Producers.Message_Producer
   is
      use PolyORB.References;
      use MOMA.Types;

      MOMA_Obj : constant MOMA.Provider.Message_Producer.Object_Acc
        := new MOMA.Provider.Message_Producer.Object;

      MOMA_Ref : PolyORB.References.Ref;
      Producer : MOMA.Message_Producers.Message_Producer;
      Type_Id_S : MOMA.Types.String
        := To_MOMA_String (Type_Id_Of (Get_Ref (Dest)));
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      --  XXX self is to be used to 'place' the receiver
      --  using session position in the POA

      MOMA_Obj.Remote_Ref := Get_Ref (Dest);
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Producer.If_Desc,
                        MOMA.Types.MOMA_Type_Id,
                        MOMA_Ref);

      Set_Destination (Producer, Dest);
      Set_Ref (Producer, MOMA_Ref);
      Set_Type_Id_Of (Producer, Type_Id_S);
      --  XXX Is it really useful to have the Ref to the remote destination in
      --  the Message_Producer itself ? By construction, this ref is
      --  encapsulated in the MOMA.Provider.Message_Producer.Object ....
      return Producer;
   end Create_Sender;

   function Create_Sender (ORB_Object : MOMA.Types.String;
                           Mesg_Pool  : MOMA.Types.String)
                           return MOMA.Message_Producers.Message_Producer
   is

      use MOMA.Types;

      use PolyORB.Annotations;
      use PolyORB.Call_Back;
      use PolyORB.References;
      use PolyORB.References.IOR;
      use PolyORB.Types;

      Producer : MOMA.Message_Producers.Message_Producer;

      ORB_Object_IOR      : constant IOR_Type := String_To_Object (ORB_Object);
      Dest_Ref_Object_IOR : constant IOR_Type := String_To_Object (Mesg_Pool);

      Type_Id_S : MOMA.Types.String
        := To_MOMA_String (Type_Id_Of (ORB_Object_IOR));

   begin
      if Type_Id_S = MOMA_Type_Id then
         raise  Program_Error;
      end if;

      Set_Ref (Producer, ORB_Object_IOR);
      Set_Type_Id_Of (Producer, Type_Id_S);
      Set_CBH (Producer, new PolyORB.Call_Back.Call_Back_Handler);
      --  XXX should free this memory sometime, somewhere ...

      Attach_Handler_To_CB
        (Call_Back_Handler (Get_CBH (Producer).all),
         MOMA.Message_Producers.Response_Handler'Access);

      Set_Note
         (Notepad_Of (Get_CBH (Producer)).all,
          CBH_Note'(Note with Dest => Dest_Ref_Object_IOR));

      return Producer;
   end Create_Sender;

   ----------------------
   -- Create_Temporary --
   ----------------------

   function Create_Temporary
     return MOMA.Destinations.Destination is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Temporary;
      pragma Warnings (On);
   end Create_Temporary;

   ---------------------
   --  Get_Transacted --
   ---------------------

   function Get_Transacted return Boolean is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Transacted;
      pragma Warnings (On);
   end Get_Transacted;

   --------------
   --  Recover --
   --------------

   procedure Recover is
   begin
      null;
      --  XXX Not Implemented
   end Recover;

   ---------------
   --  Rollback --
   ---------------

   procedure Rollback is
   begin
      null;
      --  XXX Not Implemented
   end Rollback;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Topic : MOMA.Destinations.Destination;
                        Pool  : MOMA.Destinations.Destination)
   is
   begin
      MOMA.Destinations.Subscribe (Topic, Pool, True);
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (Topic : MOMA.Destinations.Destination;
                          Pool  : MOMA.Destinations.Destination)
   is
   begin
      MOMA.Destinations.Subscribe (Topic, Pool, False);
   end Unsubscribe;

end MOMA.Sessions;

