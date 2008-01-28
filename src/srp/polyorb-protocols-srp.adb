------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P R O T O C O L S . S R P                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with Ada.Streams; use Ada.Streams;

with PolyORB.Binding_Data.Local;
with PolyORB.Filters;
with PolyORB.Filters.Iface;
with PolyORB.Log;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.ORB.Iface;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Representations.SRP;
with PolyORB.Smart_Pointers;
with PolyORB.Utils;

package body PolyORB.Protocols.SRP is

   use PolyORB.Any;
   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.ORB.Iface;
   use PolyORB.Representations.SRP;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.srp");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Rep : constant Rep_SRP_Access := new Rep_SRP;

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access SRP_Protocol;
      Session : out Filter_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);
      --  This should be factored in PolyORB.Protocols.

      Session := new SRP_Session;

      SRP_Session (Session.all).Buffer_In := new Buffers.Buffer_Type;
      SRP_Session (Session.all).Buffer_Out := new Buffers.Buffer_Type;

   end Create;

   -------------
   -- Connect --
   -------------

   procedure Connect (S : access SRP_Session) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);
      null;
   end Connect;

   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (S : access SRP_Session;
      R :  Requests.Request_Access;
      P : access Binding_Data.Profile_Type'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (R);
      pragma Unreferenced (P);
      pragma Warnings (On);
      null;
   end Invoke_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (S : access SRP_Session;
      R :  Requests.Request_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (R);
      pragma Warnings (On);
      null;
   end Abort_Request;

   ------------------
   -- Handle_Flush --
   ------------------

   procedure Handle_Flush (S : access SRP_Session) is
      pragma Unreferenced (S);

   begin
      raise Program_Error;
   end Handle_Flush;

   ----------------------
   -- Request_Received --
   ----------------------

   procedure Request_Received (S : access SRP_Session);

   procedure Request_Received (S : access SRP_Session)
   is
      use Binding_Data.Local;
      use PolyORB.Obj_Adapters;

      --  used to store the arg list needed by the method called
      Args   : Any.NVList.Ref;
      Result : Any.NamedValue;

      Deferred_Arguments_Session : Component_Access;
      ORB      : constant ORB_Access := ORB_Access (S.Server);

      Request_String : String_Ptr;
      Req    : Request_Access;

      Target_Profile : constant Binding_Data.Profile_Access
        := new Local_Profile_Type;
      Target   : References.Ref;
   begin
      --  Get the entire request string
      Request_String := new Types.String'(Unmarshall (S.Buffer_In));

      --  Split the string in its different parts and store them in
      --  a Split_SRP record
      S.SRP_Info := Split (Request_String.all);

      --  Get the arg profile needed by the method called
      Args := Obj_Adapters.Get_Empty_Arg_List
        (Object_Adapter (ORB),
         S.SRP_Info.Oid,
         To_Standard_String (S.SRP_Info.Method.all));

      if not PolyORB.Smart_Pointers.Is_Nil
        (PolyORB.Smart_Pointers.Ref (Args)) then
         --  The signature of the method is known: unmarshall
         --  the arguments right now.
         Unmarshall (Args, S.SRP_Info);
      else
         --  Unable to obtain the list of arguments at this point.
         --  Defer the unmarshalling until the Servant has a chance
         --  to provide its own arg list.
         Deferred_Arguments_Session
           := Components.Component_Access (S);
      end if;

      --  Get the result profile for the method called and create an
      --  appropriate Any.NamedValue for the result
      --  Result := (Name     => To_PolyORB_String ("Result"),
      --           Argument =>  Obj_Adapters.Get_Empty_Result
      --             (Object_Adapter (ORB), SRP_Info.Oid,
      --              To_Standard_String (SRP_Info.Method.all)),
      --           Arg_Modes => 0);

      --  Create a local profile for the request. Indeed, the request isnnow
      --  local
      Create_Local_Profile
        (S.SRP_Info.Oid.all, Local_Profile_Type (Target_Profile.all));
      References.Create_Reference ((1 => Target_Profile), "", Target);

      --  Create a Request
      Create_Request
        (Target    => Target,
         Operation => To_Standard_String (S.SRP_Info.Method.all),
         Arg_List  => Args,
         Result    => Result,
         Deferred_Arguments_Session => Deferred_Arguments_Session,
         Req       => Req,
         Dependent_Binding_Object =>
           Smart_Pointers.Entity_Ptr
         (S.Dependent_Binding_Object));

      --  Queue the request for execution

      Queue_Request_To_Handler (ORB,
        Queue_Request'
          (Request   => Req,
           Requestor => Component_Access (S)));

   end Request_Received;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply (S : access SRP_Session; R : Request_Access) is
      SRP_Info : Split_SRP;
      B : Buffer_Access renames S.Buffer_Out;
   begin
      Release_Contents (B.all);
      Set_SRP_Method (To_PolyORB_String ("Reply"), SRP_Info);
      Set_SRP_Oid (Object_Id'(1 .. 4 => 0), SRP_Info);
      Set_SRP_Arg (To_PolyORB_String ("Data"),
                   To_Any (To_PolyORB_String
                           ("200 OK" & Image (R.all)
                            & " " & PolyORB.Any.Image (R.Result))),
                   SRP_Info);

      --  Data := Join (SRP_Info);

      --  XXX Before using this procedure, we must be able to
      --  [un]marshall Split_SRP [from] to Any
      --  Marshall_From_Any (Rep.all, B, Data);

      Marshall_From_Split_SRP (Rep.all, B, SRP_Info);

      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => B));
   end Send_Reply;

   -------------------------------
   -- Handle_Connect_Indication --
   -------------------------------

   procedure Handle_Connect_Indication (S : access SRP_Session) is
   begin
      pragma Debug (C, O ("Received new connection to SRP service..."));

      --  1. Send greetings to client.

      --  Send_String ("Hello, please type data." & ASCII.LF);

      --  2. Notify transport layer that more data is expected.

      Expect_Data (S, S.Buffer_In, 1024);
      --  Exact => False

      --  Note that there is no race condition here. One might
      --  expect the following unfortunate sequence of events:
      --    10. Greetings sent to client
      --    11. Client answers
      --    20. Expect_Data
      --  (in 11: transport gets unexpected data).
      --  This does not actually happen because the TE is not
      --  being monitored while Send_Greetings and Expect_Data
      --  are done; it becomes monitored again /after/ the
      --  Connect_Indication has been processed.
      --
      --  The same goes for the handling of a Data_Indication.

   end Handle_Connect_Indication;

   ---------------------------------
   -- Handle_Connect_Confirmation --
   ---------------------------------

   procedure Handle_Connect_Confirmation (S : access SRP_Session) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);
      null;
      --  No setup is necessary for newly-created client connections.
   end Handle_Connect_Confirmation;

   ----------------------------
   -- Handle_Data_Indication --
   ----------------------------

   procedure Handle_Data_Indication
     (S           : access SRP_Session;
      Data_Amount : Stream_Element_Count;
      Error       : in out Errors.Error_Container)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Data_Amount, Error);
      pragma Warnings (On);

      pragma Debug (C, O ("Received data on SRP service..."));
      pragma Debug (Buffers.Show (S.Buffer_In));

      Request_Received (S);

      Buffers.Release_Contents (S.Buffer_In.all);
      --  Clean up

      Expect_Data (S, S.Buffer_In, 1024);
      --  XXX DUMMY size
   end Handle_Data_Indication;

   -----------------------
   -- Handle_Disconnect --
   -----------------------

   procedure Handle_Disconnect
     (S : access SRP_Session; Error : Errors.Error_Container)
   is
      pragma Unreferenced (Error);
   begin
      pragma Debug (C, O ("Received disconnect."));

      --  Cleanup protocol.
      Buffers.Release (S.Buffer_In);

   end Handle_Disconnect;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (Ses   : access SRP_Session;
      Args  : in out Any.NVList.Ref;
      Error : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);

   begin
      Unmarshall (Args, Ses.SRP_Info);
   end Handle_Unmarshall_Arguments;

   --------------------------------
   -- Unmarshall_Request_Message --
   --------------------------------

   procedure Unmarshall_Request_Message (Buffer : access Buffer_Type;
                                         Oid    : access Object_Id;
                                         Method : access Types.String) is
   begin
      Method.all := Unmarshall (Buffer);

      declare
         Obj : constant Stream_Element_Array := Unmarshall (Buffer);
      begin
         Oid.all := Object_Id (Obj);
      end;
   end Unmarshall_Request_Message;

   ---------------------
   -- Unmarshall_Args --
   ---------------------

   procedure Unmarshall_Args (Buffer : access Buffer_Type;
                              Args   : in out Any.NVList.Ref)
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      It : Iterator := First (List_Of (Args).all);
   begin
      --  By modifing Args_list, we modify directly Args
      while not Last (It) loop
         Unmarshall (Buffer, Value (It).all);
         Next (It);
      end loop;
   end Unmarshall_Args;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall (Args : in out Any.NVList.Ref; SRP_Info : Split_SRP)
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Utils;

      function To_SEA (S : Types.String) return Stream_Element_Array;

      function To_SEA (S : Types.String) return Stream_Element_Array
      is
         Temp_S : constant Standard.String := To_Standard_String (S);
         Value  : Stream_Element_Array (1 .. Temp_S'Length);
      begin
         for I in Value'Range loop
            Value (I) := Stream_Element
              (Character'Pos (Temp_S (Temp_S'First +
                                      Integer (I - Value'First))));
         end loop;
         return Value;
      end To_SEA;

      It : Iterator := First (List_Of (Args).all);
      Current_Arg : Arg_Info_Ptr := SRP_Info.Args;
      Temp_Arg  : Element_Access;

      Temp_Buffer : aliased Buffer_Type;
      --  XXX BAD BAD buffer allocated on the stack
   begin
      while not Last (It) loop
         Temp_Arg := Value (It);

         declare
            Value : aliased Stream_Element_Array :=
              To_SEA (Current_Arg.all.Value.all & ASCII.nul);
         begin
            Initialize_Buffer (Buffer     => Temp_Buffer'Access,
                               Size       => Value'Length,
                               Data       => Value (Value'First)'Address,
                               Endianness => Little_Endian,
                               Initial_CDR_Position => 0);
            Show (Temp_Buffer'Access);
            Unmarshall (Temp_Buffer'Access, Temp_Arg.all);
         end;
         Next (It);
         Current_Arg := Current_Arg.all.Next;
      end loop;
   end Unmarshall;

end PolyORB.Protocols.SRP;
