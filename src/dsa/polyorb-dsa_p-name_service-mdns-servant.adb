------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.SERVANT                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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

with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Any;

with PolyORB.References.Corbaloc;

package body PolyORB.DSA_P.Name_Service.mDNS.Servant is
   use PolyORB.Errors;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.dsa_p.name_service.mdns.servant");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Query (Self : access Object;
                    Authoritative : in out Boolean;
                    Question : rrSequence;
                    Answer : out rrSequence;
                    Authority : out rrSequence;
                    Additional : out rrSequence;
                    Response : out Rcode)
   is
      pragma Unreferenced (Self);
   begin

      --  by default, each partition is autoritative
      Authoritative := True;

      --  for each received question we must look for an RR or a list of RRs
      --  and assign them to the RR answer/additional infos sequence
      for J in 1 .. Length (Question) loop
         Find_Answer_RR (Get_Element (Question, J),
                         Answer, Authority, Additional, Response);
      end loop;

   end Query;

   procedure Invoke
     (Self    : access Object;
      Request : PolyORB.Requests.Request_Access)
   is
      use PolyORB.Any.NVList;
      Operation : Standard.String renames Request.all.Operation.all;

      Arg_List    : PolyORB.Any.NVList.Ref;
   begin
      pragma Debug (C, O ("The dns servant is executing the request:"
                    & PolyORB.Requests.Image (Request.all)));

      Create (Arg_List);

      if Operation = "Query" then

         declare
            use PolyORB.Requests;
            use PolyORB.Any;
            use PolyORB.Any.NVList.Internals;
            use PolyORB.Any.NVList.Internals.NV_Lists;

            Argument_Authoritative : constant PolyORB.Any.Any :=
              PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Boolean);
            Argument_Question  : constant PolyORB.Any.Any :=
              PolyORB.Any.Get_Empty_Any (TC_IDL_SEQUENCE_DNS_RR);
            Argument_Answer : PolyORB.Any.Any :=
              PolyORB.Any.Get_Empty_Any (TC_IDL_SEQUENCE_DNS_RR);
            Argument_Authority : PolyORB.Any.Any :=
              PolyORB.Any.Get_Empty_Any (TC_IDL_SEQUENCE_DNS_RR);

            Argument_Additional : PolyORB.Any.Any :=
               PolyORB.Any.Get_Empty_Any (TC_IDL_SEQUENCE_DNS_RR);

            Question : rrSequence;
            authoritative : PolyORB.Types.Boolean;
            answer       : rrSequence;
            authority    : rrSequence;
            additional   : rrSequence;
            Result          : Rcode;
            Exception_Error : Error_Container;

         begin
            --  Create argument list
            pragma Debug (C, O ("Creating argument list"));
            Add_Item (Arg_List,
                      Arg_Name_Auth,
                      Argument_Authoritative,
                      PolyORB.Any.ARG_INOUT);
            Add_Item (Arg_List,
                      Arg_Name_Question,
                      Argument_Question,
                      PolyORB.Any.ARG_IN);

            Add_Item (Arg_List,
                      Arg_Name_Answer,
                      Argument_Answer,
                      PolyORB.Any.ARG_OUT);

            Add_Item (Arg_List,
                      Arg_Name_Au,
                      Argument_Authority,
                      PolyORB.Any.ARG_OUT);
            Add_Item (Arg_List,
                      Arg_Name_Add,
                      Argument_Additional,
                      PolyORB.Any.ARG_OUT);

            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
            end if;

            --  retrieving IN arguments from the Any representation
            authoritative :=  PolyORB.Any.From_Any (Argument_Authoritative);
            Question := To_Sequence (1);
            Question := From_Any (Argument_Question);

            Query (Self,
                   authoritative,
                   Question,
                   answer,
                   authority,
                   additional,
                   Result);

            --  Convertion the out rr sequences to the Any type
            Argument_Answer := To_Any (answer);
            Argument_Authority := To_Any (authority);
            Argument_Additional := To_Any (additional);

            --  Setting out args
            declare
               It  : Iterator := First (List_Of (Request.Out_Args).all);
               Arg : Element_Access;
            begin
               Arg := Value (It);
               Set_Any_Value (authoritative,
                            Get_Container (Arg.Argument).all);
               Next (It);
               Arg := Value (It);
               Copy_Any_Value (Arg.Argument, Argument_Question);
               --  answer rr sequence
               Next (It);
               Arg := Value (It);
               Copy_Any_Value (Arg.Argument, Argument_Answer);
               --  authority servers rr sequence
               Next (It);
               Arg := Value (It);
               Copy_Any_Value (Arg.Argument, Argument_Authority);
               --  additionnal info rr sequence
               Next (It);
               Arg := Value (It);
               Copy_Any_Value (Arg.Argument, Argument_Additional);
            end;

            PolyORB.Requests.Set_Result (Request, To_Any (Result));

            return;
         end;

      end if;
   end Invoke;

   procedure Find_Answer_RR (Question       : RR;
                             Answer_Seq     : out rrSequence;
                             Authority_Seq  : out rrSequence;
                             Additional_Seq : out rrSequence;
                             Response       : out Rcode)
   is

      TTL : constant PolyORB.Types.Unsigned_Long := 240;
      Answer : RR;
      SRV_RR : SRV_Data;
      Version : PolyORB.Types.String;
      Current_Entry : Local_Entry_Ptr;
      pragma Unreferenced (Authority_Seq);

   begin

      case Question.rr_type is

         --  Currently, the protocol for exchanging mDNS messages in the
         --  in the mDNS context implies the usage of the SRV/TXT mapping,
         --  so upon reception of an SRV message, we generate an answer
         --  resource record by looking up the Local_Entry_List and we assign
         --  the necessary data to  the SRV record (stored in the
         --  Answer rr sequence) and the TXT record (stored in the Additional
         --  rr sequence).

         when SRV =>

            --  xxx: concurent access mutex locks?

            Current_Entry := Local_Entry_List.Lookup
              (Types.To_Standard_String (Question.rr_name), null);

            if Current_Entry = null then

               --  If the record is not found locally, we return a
               --  Name_Error DNS Rcode to client.

               Response := Name_Error;

               return;
            end if;

            Answer_Seq := To_Sequence (1);
            Additional_Seq := To_Sequence (1);
            Answer.rr_name := Question.rr_name;
            Answer.TTL := TTL;
            Answer.rr_type := SRV;

            declare
               SRV_RR_Data   : RR_Data (Answer.rr_type);
            begin

               Version := Current_Entry.Version;

               declare
                  Base_Ref_String : constant String :=
                    PolyORB.References.Corbaloc.Object_To_String
                      (Current_Entry.Base_Ref);
               begin
                  SRV_RR.target :=
                    PolyORB.Types.To_PolyORB_String (Base_Ref_String);
               end;

               SRV_RR.priority := PolyORB.Types.Unsigned_Short (0);
               SRV_RR.weight := PolyORB.Types.Unsigned_Short (0);
               SRV_RR.port := PolyORB.Types.Unsigned_Short (0);
               --  currently default values

               Answer.data_length := PolyORB.Types.Unsigned_Short
                (8 + PolyORB.Types.To_Standard_String (SRV_RR.target)'Length);
               SRV_RR_Data.srv_data := SRV_RR;
               Answer.rr_data := SRV_RR_Data;
            end;

            Replace_Element (Answer_Seq, 1, Answer);
            Answer.rr_name := SRV_RR.target;
            Answer.rr_type := TXT;

            declare
               TXT_RR_Data : RR_Data (Answer.rr_type);
            begin
               TXT_RR_Data.rr_answer := PolyORB.Types.To_PolyORB_String
                 ("version=" & PolyORB.Types.To_Standard_String (Version));

               Answer.data_length := PolyORB.Types.Unsigned_Short
                 (2 + PolyORB.Types.To_Standard_String
                    (TXT_RR_Data.rr_answer)'Length);
               Answer.rr_data := TXT_RR_Data;
            end;
            Replace_Element (Additional_Seq, 1, Answer);
            Response := No_Error;
         --  XXX:The following RRs are used for testing purposes currently
         when A =>
            Answer.rr_name := Question.rr_name;
            Answer.TTL := TTL;
            Answer.rr_type := A;
            declare
               rd : RR_Data (Answer.rr_type);
               addr : IDL_AT_Sequence_4_octet;
            begin
               addr :=
                 IDL_AT_Sequence_4_octet (IDL_SEQUENCE_4_octet.To_Sequence
                 (IDL_SEQUENCE_4_octet.Element_Array'(192, 168, 1, 11)));
               rd.a_address := addr;
               Answer.data_length := PolyORB.Types.Unsigned_Short (4);
               Answer.rr_data := rd;
            end;
         when PTR =>
            Answer_Seq := To_Sequence (1);
            Answer.rr_name := Question.rr_name;
            Answer.rr_type := PTR;
            Answer.TTL := TTL;
            declare
               rd : RR_Data (Answer.rr_type);
            begin
               rd.rr_answer := PolyORB.Types.To_PolyORB_String ("TEST_PTR");
               Answer.data_length := PolyORB.Types.Unsigned_Short
                 (2 + PolyORB.Types.To_Standard_String (rd.rr_answer)'Length);
               Answer.rr_data := rd;
               Replace_Element (Answer_Seq, 1, Answer);
            end;
         when TXT =>
            Answer_Seq := To_Sequence (1);
            Answer.rr_name := Question.rr_name;
            Answer.TTL := TTL;
            Answer.rr_type := TXT;
            declare
               rd : RR_Data (Answer.rr_type);
            begin
               rd.rr_answer := PolyORB.Types.To_PolyORB_String
                           ("TEST FOR TXT RECORD");
               Answer.data_length := PolyORB.Types.Unsigned_Short
               (2 + PolyORB.Types.To_Standard_String (rd.rr_answer)'Length);
               Answer.rr_data := rd;
            end;
            Replace_Element (Answer_Seq, 1, Answer);

            --  Other RR types are not currently supported
         when others =>
               raise Program_Error;
      end case;
   end Find_Answer_RR;

   procedure Append_Entry_To_Context (Name     : PolyORB.Types.String;
                                      Kind     : PolyORB.Types.String;
                                      Version  : PolyORB.Types.String;
                                      Base_Ref : PolyORB.References.Ref)
   is
      New_Entry : Local_Entry_Ptr;
   begin
      pragma Debug (C, O ("Appending new entry "));
      New_Entry := new Local_Entry;
      New_Entry.Name := Name;
      New_Entry.Kind := Kind;
      New_Entry.Version := Version;
      New_Entry.Base_Ref := Base_Ref;

      --  XXX: needs a mutex?
      Local_Entry_List.Register (PolyORB.Types.To_Standard_String
                                 (Name), New_Entry);
      pragma Debug (C, O ("Entry Appended : leaving"));
   end Append_Entry_To_Context;

end PolyORB.DSA_P.Name_Service.mDNS.Servant;
