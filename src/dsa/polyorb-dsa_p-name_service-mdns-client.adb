------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.CLIENT                   --
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
with Ada.Characters.Handling;
with PolyORB.Any.NVList;
with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Log;
with PolyORB.Utils;

package body PolyORB.DSA_P.Name_Service.mDNS.Client is
   use PolyORB.DSA_P.Name_Service.mDNS.Helper;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.dsa_p.name_service.mdns.client");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   function To_Lower (S : String) return String
     renames Ada.Characters.Handling.To_Lower;

   Query_Arg_Name_Authoritative : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Authoritative");

   Query_Arg_Name_Question : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Question");

   Query_Arg_Name_Answer : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Answer");

   Query_Arg_Name_Authority : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Authority");

   Query_Arg_Name_Additional : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Additional");

   Query_Result_Name : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   --------------------
   -- Query_Result --
   --------------------
   function Query_Result return PolyORB.Any.NamedValue;
   function Query_Result return PolyORB.Any.NamedValue is
      pragma Inline (Query_Result);
   begin
      return (Name => Query_Result_Name,
      Argument => PolyORB.Any.Get_Empty_Any
        (mDNS.Helper.TC_Rcode),
      Arg_Modes => 0);
   end Query_Result;

   function Get_Wrapper_Any
        (TC : PolyORB.Any.TypeCode.Local_Ref;
         CC : access PolyORB.Any.Content'Class) return PolyORB.Any.Any;

   function Get_Wrapper_Any
        (TC : PolyORB.Any.TypeCode.Local_Ref;
         CC : access PolyORB.Any.Content'Class) return PolyORB.Any.Any
   is
      Result : constant PolyORB.Any.Any := PolyORB.Any.Get_Empty_Any (TC);
      pragma Suppress (Accessibility_Check);
   begin
      PolyORB.Any.Set_Value (PolyORB.Any.Get_Container (Result).all,
                               PolyORB.Any.Content_Ptr (CC));
      return Result;
   end Get_Wrapper_Any;

   -------------
   -- Resolve --
   -------------
   --  Sending an mdns message on the local link to retrieve
   --  a valid reference of the selected unit

   function Resolve
     (The_Ref : PolyORB.References.Ref;
      Name    : String;
      Kind    : String)
      return PolyORB.References.Ref
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Types;

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;

      Question_RR : RR;
      Q_sequence : rrSequence;
      A_sequence : rrSequence;
      Auth_sequence : rrSequence;
      Add_sequence : rrSequence;
      Authoritative : Types.Boolean;

      Res : Rcode;
      Answer_rr, Add_rr : RR;
      Version_Id : PolyORB.Types.String;
      Ref : PolyORB.References.Ref;
   begin
      pragma Debug (C, O ("Enter Resolve : sending to :"
        & PolyORB.References.Image (The_Ref)));

      Authoritative := False;

      --  Create argument list
      Q_sequence := To_Sequence (1);
      PolyORB.Any.NVList.Create
        (Arg_List);
      Question_RR.rr_name := To_PolyORB_String (Name & "._" & To_Lower (Kind));
      Question_RR.rr_type := SRV;
      Question_RR.TTL := 240;
      Replace_Element (Q_sequence, 1, Question_RR);

      --  calling the query procedure
      Query
        (Self          => The_Ref,
         Authoritative => Authoritative,
         Question      => Q_sequence,
         Answer        => A_sequence,
         Authority     => Auth_sequence,
         Additional    => Add_sequence,
         Returns       => Res);

      --  If the the object has been successfully found
      --  and the out arguments received we receive a No_Error Rcode.

      if Res = No_Error then

         Answer_rr := Get_Element (A_sequence, 1);

         pragma Debug (C, O ("Answer: " &
           PolyORB.Types.To_Standard_String (Answer_rr.rr_name)));

         pragma Debug (C, O ("Target: " &
           PolyORB.Types.To_Standard_String
             (Answer_rr.rr_data.srv_data.target)));

         Add_rr := Get_Element (Add_sequence, 1);

         if Add_rr.rr_type = TXT then
            Parse_TXT_Record (Add_rr.rr_data.rr_answer, Version_Id);

            --  Creating a reference form its stringified representation

            PolyORB.References.String_To_Object
              (Types.To_Standard_String (Answer_rr.rr_data.srv_data.target),
               Ref);

            --  Setting its type id, for version checking purposes

            if PolyORB.References.Type_Id_Of (Ref) = "" then
               PolyORB.References.Set_Type_Id
                 (Ref, "DSA:" & To_Standard_String (Answer_rr.rr_name) & ":"
                    & To_Standard_String (Version_Id));
            end if;

         end if;
      elsif Res = Name_Error then
         pragma Debug (C, O ("Record was not found"));
         null;
      else
         raise Program_Error;
      end if;

      PolyORB.Requests.Destroy_Request (Request);
      --  Request has been synchronously invoked

      return Ref;
   end Resolve;

   -----------
   -- Query --
   -----------

   procedure Query
     (Self : PolyORB.References.Ref;
      Authoritative : in out PolyORB.Types.Boolean;
      Question : PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Answer : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Authority : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Additional : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Returns : out PolyORB.DSA_P.Name_Service.mDNS.Helper.Rcode)
   is
      Argument_List : PolyORB.Any.NVList.Ref;
      Result : PolyORB.DSA_P.Name_Service.mDNS.Helper.Rcode
        renames Returns;

      Arg_CC_Result : aliased PolyORB.Any.Content'Class :=
        mDNS.Helper.Internals.Wrap
           (Result'Unrestricted_Access);
      Arg_CC_Authoritative : aliased PolyORB.Any.Content'Class :=
        PolyORB.Any.Wrap
           (Authoritative'Unrestricted_Access);
      Arg_Any_Authoritative : constant PolyORB.Any.Any :=

      Get_Wrapper_Any
           (PolyORB.Any.TC_Boolean,
            Arg_CC_Authoritative'Unchecked_Access);

      Arg_CC_Question : aliased PolyORB.Any.Content'Class :=
        PolyORB.DSA_P.Name_Service.mDNS.Helper.Internals.Wrap
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.IDL_SEQUENCE_DNS_RR.Sequence
              (Question)'Unrestricted_Access);
      Arg_Any_Question : constant PolyORB.Any.Any :=
       Get_Wrapper_Any
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.TC_rrSequence,
            Arg_CC_Question'Unchecked_Access);
      Arg_CC_Answer : aliased PolyORB.Any.Content'Class :=
        PolyORB.DSA_P.Name_Service.mDNS.Helper.Internals.Wrap
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.IDL_SEQUENCE_DNS_RR.Sequence
              (Answer)'Unrestricted_Access);
      Arg_Any_Answer : constant PolyORB.Any.Any :=
        Get_Wrapper_Any
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.TC_rrSequence,
            Arg_CC_Answer'Unchecked_Access);
      pragma Warnings (Off, Answer);
      Arg_CC_Authority : aliased PolyORB.Any.Content'Class :=
        PolyORB.DSA_P.Name_Service.mDNS.Helper.Internals.Wrap
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.IDL_SEQUENCE_DNS_RR.Sequence
              (Authority)'Unrestricted_Access);
      Arg_Any_Authority : constant PolyORB.Any.Any :=
        Get_Wrapper_Any
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.TC_rrSequence,
            Arg_CC_Authority'Unchecked_Access);
      pragma Warnings (Off, Authority);
      Arg_CC_Additional : aliased PolyORB.Any.Content'Class :=
        PolyORB.DSA_P.Name_Service.mDNS.Helper.Internals.Wrap
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.IDL_SEQUENCE_DNS_RR.Sequence
              (Additional)'Unrestricted_Access);
      Arg_Any_Additional : constant PolyORB.Any.Any :=
        Get_Wrapper_Any
           (PolyORB.DSA_P.Name_Service.mDNS.Helper.TC_rrSequence,
            Arg_CC_Additional'Unchecked_Access);
      pragma Warnings (Off, Additional);
      Request : PolyORB.Requests.Request_Access;
      Result_Nv : PolyORB.Any.NamedValue :=
        Query_Result;
   begin
      pragma Debug (C, O ("Query : enter"));
      if Self.Is_Nil then
         pragma Debug (C, O ("Query : Ref is nil"));
         raise Program_Error;
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List,
         Query_Arg_Name_Authoritative,
         Arg_Any_Authoritative,
         PolyORB.Any.ARG_INOUT);

      PolyORB.Any.NVList.Add_Item
        (Argument_List,
         Query_Arg_Name_Question,
           Arg_Any_Question,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List,
         Query_Arg_Name_Answer,
         Arg_Any_Answer,
         PolyORB.Any.ARG_OUT);
      PolyORB.Any.NVList.Add_Item
        (Argument_List,
         Query_Arg_Name_Authority,
           Arg_Any_Authority,
         PolyORB.Any.ARG_OUT);
      PolyORB.Any.NVList.Add_Item
        (Argument_List,
         Query_Arg_Name_Additional,
           Arg_Any_Additional,
         PolyORB.Any.ARG_OUT);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv.Argument).all,
         Arg_CC_Result'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => Self,
         Operation => "Query",
         Arg_List => Argument_List,
         Req_Flags => PolyORB.Requests.Sync_With_Target,
         Result => Result_Nv,
         Req => Request);
      --  Invoking the request (synchronously or asynchronously)

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);
   end Query;

   procedure Parse_TXT_Record (Answer_RR : PolyORB.Types.String;
                               Version_id : out PolyORB.Types.String) is
      use PolyORB.Utils;
      S : constant String :=
        PolyORB.Types.To_Standard_String (Answer_RR);
      Index : Integer;
      Index2 : Integer;
   begin
      Index  := Find (S, S'First, '=') + 1;
      Index2 := S'Last;
      Version_id := Types.To_PolyORB_String (S (Index .. Index2));
   end Parse_TXT_Record;

end PolyORB.DSA_P.Name_Service.mDNS.Client;
