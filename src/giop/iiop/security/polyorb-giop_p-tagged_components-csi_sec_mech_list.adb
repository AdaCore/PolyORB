------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           POLYORB.GIOP_P.TAGGED_COMPONENTS.CSI_SEC_MECH_LIST             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Annotations;
with PolyORB.Binding_Data.GIOP.IIOP;
with PolyORB.Binding_Data_QoS;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.GIOP_P.Tagged_Components.Null_Tag;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.Protocols.GIOP;
with PolyORB.QoS.Clients_Security;
with PolyORB.QoS.Targets_Security;
with PolyORB.QoS.Transport_Contexts;
with PolyORB.Representations.CDR.Common;
with PolyORB.Security.Authentication_Mechanisms;
with PolyORB.Security.Backward_Trust_Evaluators;
with PolyORB.Security.Forward_Trust_Evaluators;
with PolyORB.Setup;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List is

   use Ada.Streams;
   use PolyORB.Annotations;
   use PolyORB.ASN1;
   use PolyORB.Binding_Data.GIOP;
   use PolyORB.Errors;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.QoS;
   use PolyORB.QoS.Clients_Security;
   use PolyORB.QoS.Clients_Security.Client_Mechanism_Lists;
   use PolyORB.QoS.Targets_Security;
   use PolyORB.QoS.Targets_Security.Target_Mechanism_Lists;
   use PolyORB.QoS.Transport_Contexts;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Security.Authentication_Mechanisms;
   use PolyORB.Security.Authority_Mechanisms;
   use PolyORB.Security.Authority_Mechanisms.Client_Authority_Mechanism_Lists;
   use PolyORB.Security.Authority_Mechanisms.Target_Authority_Mechanism_Lists;
   use PolyORB.Security.Backward_Trust_Evaluators;
   use PolyORB.Security.Forward_Trust_Evaluators;
   use PolyORB.Security.Exported_Names;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.Security.Types;
   use PolyORB.Security.Types.OID_Lists;
   use PolyORB.Types;

   use Mechanism_Lists;
   use Service_Configuration_Lists;

   type Compound_Mechanism_Note is new PolyORB.Annotations.Note with record
      Mechanism : Mechanism_Access;
   end record;

   type Registry_Record is record
      Tag            : Tag_Value;
      TC_Constructor : To_Tagged_Component;
      TM_Constructor : To_Security_Transport_Mechanism;
   end record;

   package Registry_Lists is new PolyORB.Utils.Chained_Lists (Registry_Record);

   procedure Release_Contents (X : in out Mechanism);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Item   :        Mechanism);

   function Unmarshall (Buffer : access Buffer_Type) return Mechanism;

   --  WAG:504 need fully qualified name for visibility on sibling of parent
   --  (PolyORB.Binding_Data).

   procedure Create_Transport_Mechanisms
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : PolyORB.Binding_Data.Profile_Access;
      Mechs   : in out Transport_Mechanism_List);

   procedure Fetch_QoS
     (P : access PolyORB.Binding_Data.GIOP.IIOP.IIOP_Profile_Type);

   function Fetch_Tagged_Component
     (Oid : PolyORB.Objects.Object_Id) return Tagged_Component_Access;

   function Fetch_QoS
     (End_Point : PolyORB.Transport.Transport_Endpoint_Access)
      return PolyORB.QoS.QoS_Parameter_Access;

   function Is_Selected
     (QoS       : PolyORB.QoS.QoS_Parameters;
      Mechanism :
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Access)
     return Boolean;

   function Deep_Copy
     (Item : Mechanism_Lists.List) return Mechanism_Lists.List;

   function Deep_Copy (Item : Service_Configuration_Lists.List)
      return Service_Configuration_Lists.List;

   function Duplicate (Item : Mechanism_Access) return Mechanism_Access;

   procedure Initialize;

   function Create_Empty_Component return Tagged_Component_Access;

   Registry : Registry_Lists.List;

   QoS_Registry : QoS_Constructor := null;
   --  XXX Only one secure transportm mechanism supported for now

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_CSI_Sec_Mech_List;
   end Create_Empty_Component;

   ---------------------------------
   -- Create_Transport_Mechanisms --
   ---------------------------------

   --  WAG:504 need fully qualified name for visibility on sibling of parent
   --  (PolyORB.Binding_Data).

   procedure Create_Transport_Mechanisms
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : PolyORB.Binding_Data.Profile_Access;
      Mechs   : in out Transport_Mechanism_List)
   is
      Iter   : Mechanism_Lists.Iterator :=
                 First (TC_CSI_Sec_Mech_List (TC.all).Mechanisms);

   begin
      while not Last (Iter) loop
         declare
            TCL : Tagged_Component_List;
            TML : Transport_Mechanism_List;
            TMI : Transport_Mechanism_Lists.Iterator;
            use Transport_Mechanism_Lists;
         begin
            Append (TCL, Value (Iter).all.Transport_Mechanism_Tag);
            Create_Transport_Mechanisms (TCL, Profile, TML);
            --  possible memory leak, when is TML deallocated???
            Deallocate (TCL);

            if Length (TML) = 0 then
               Value (Iter).all.Transport_Mechanism := null;

            elsif Length (TML) = 1 then
               TMI := First (TML);
               if Value (TMI).all /= Get_Primary_Transport_Mechanism
                                       (GIOP_Profile_Type (Profile.all))
               then
                  Value (Iter).all.Transport_Mechanism := Element (TML, 0).all;
                  Append (Mechs, Value (TMI).all);

               else
                  Value (Iter).all.Transport_Mechanism :=
                    Get_Primary_Transport_Mechanism
                      (GIOP_Profile_Type (Profile.all));
               end if;

            else
               raise Program_Error;
            end if;
         end;

         Next (Iter);
      end loop;
   end Create_Transport_Mechanisms;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (Item : Mechanism_Lists.List) return Mechanism_Lists.List
   is
      Result : Mechanism_Lists.List;
      Iter   : Mechanism_Lists.Iterator := Mechanism_Lists.First (Item);

   begin
      while not Mechanism_Lists.Last (Iter) loop
         Mechanism_Lists.Append
           (Result,
            Duplicate (Mechanism_Lists.Value (Iter).all));

         Mechanism_Lists.Next (Iter);
      end loop;

      return Result;
   end Deep_Copy;

   function Deep_Copy (Item : Service_Configuration_Lists.List)
      return Service_Configuration_Lists.List
   is
      Result : Service_Configuration_Lists.List;
      Iter   : Service_Configuration_Lists.Iterator
        := Service_Configuration_Lists.First (Item);

   begin
      while not Service_Configuration_Lists.Last (Iter) loop
         Service_Configuration_Lists.Append
           (Result,
            (Service_Configuration_Lists.Value (Iter).all.Syntax,
             new Ada.Streams.Stream_Element_Array'
             (Service_Configuration_Lists.Value (Iter).all.Name.all)));

         Service_Configuration_Lists.Next (Iter);
      end loop;

      return Result;
   end Deep_Copy;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (Item : Mechanism_Access) return Mechanism_Access is
      Result : constant Mechanism_Access := new Mechanism;

   begin
      Result.Target_Requires := Item.Target_Requires;
      Result.Transport_Mechanism_Tag :=
         Duplicate (Item.Transport_Mechanism_Tag.all);
      Result.Authentication_Target_Supports :=
         Item.Authentication_Target_Supports;
      Result.Authentication_Target_Requires :=
         Item.Authentication_Target_Requires;
      Result.Authentication_Mechanism :=
         Duplicate (Item.Authentication_Mechanism);

      if Item.Authentication_Target_Name /= null then
         Result.Authentication_Target_Name :=
            Duplicate (Item.Authentication_Target_Name);
      end if;

      Result.Attribute_Target_Supports := Item.Attribute_Target_Supports;
      Result.Attribute_Target_Requires := Item.Attribute_Target_Requires;
      Result.Attribute_Privilege_Authorities :=
         Deep_Copy (Item.Attribute_Privilege_Authorities);
      Result.Attribute_Naming_Mechanisms :=
         PolyORB.Security.Types.Duplicate (Item.Attribute_Naming_Mechanisms);
      Result.Attribute_Identity_Types := Item.Attribute_Identity_Types;

      return Result;
   end Duplicate;

   overriding function Duplicate (C : TC_CSI_Sec_Mech_List)
     return Tagged_Component_Access
   is
      Result : constant TC_CSI_Sec_Mech_List_Access
        := new TC_CSI_Sec_Mech_List;

   begin
      Result.Stateful := C.Stateful;
      Result.Mechanisms := Deep_Copy (C.Mechanisms);

      return Tagged_Component_Access (Result);
   end Duplicate;

   ---------------
   -- Fetch_QoS --
   ---------------

   procedure Fetch_QoS
     (P : access PolyORB.Binding_Data.GIOP.IIOP.IIOP_Profile_Type)
   is
      use PolyORB.Binding_Data_QoS;

      C : constant TC_CSI_Sec_Mech_List_Access :=
        TC_CSI_Sec_Mech_List_Access
        (PolyORB.Binding_Data.GIOP.Get_Component
         (GIOP_Profile_Type (P.all), Tag_CSI_Sec_Mech_List));

   begin
      if C = null then
         return;
      end if;

      declare
         QoS  : constant QoS_Client_Security_Parameter_Access
           := new QoS_Client_Security_Parameter;
         Iter : Mechanism_Lists.Iterator := First (C.Mechanisms);

      begin
         --  If target supports unprotected invocations then add
         --  compound mechanism for unprotected invocations

--         Append (QoS.Mechanisms, new Client_Mechanism);

         --  XXX For now, always add unprotected mechanism.

         --  Add supported by client protected compound mechanisms

         while not Last (Iter) loop
            declare
               Aux       : Client_Mechanism_Access := new Client_Mechanism;
               Supported : Boolean                 := True;
               Note      : Compound_Mechanism_Note;

            begin
               Aux.Stateful := C.Stateful;

               --  Transport Mechanism

               if Value (Iter).all.Transport_Mechanism_Tag = null then
                  raise Program_Error;

               else
                  Supported := False;

                  declare
                     use Registry_Lists;

                     C_Iter : Registry_Lists.Iterator := First (Registry);

                  begin
                     while not Last (C_Iter) loop
                        if Value (C_Iter).all.Tag
                             = Value (Iter).all.Transport_Mechanism_Tag.Tag
                        then
                           Aux.Transport :=
                             Value (C_Iter).all.TM_Constructor
                             (Value (Iter).all.Transport_Mechanism_Tag);

                           Supported := True;

                           exit;
                        end if;

                        Next (C_Iter);
                     end loop;
                  end;
               end if;

               --  Authentication

               if Is_Set
                   (Establish_Trust_In_Client,
                    Value (Iter).all.Authentication_Target_Supports)
               then
                  Aux.Authentication_Mechanism :=
                    Create_Client_Mechanism
                    (Duplicate (Value (Iter).all.Authentication_Mechanism),
                     Duplicate
                     (Value (Iter).all.Authentication_Target_Name));
                  Aux.Authentication_Required :=
                     Is_Set
                     (Establish_Trust_In_Client,
                      Value (Iter).all.Authentication_Target_Requires);

                  if Aux.Authentication_Mechanism = null then
                     Supported := False;
                  end if;

               else
                  Aux.Authentication_Mechanism := null;
                  Aux.Authentication_Required  := False;
               end if;

               --  Identity Assertion

               Aux.Identity_Assertion :=
                 Is_Set
                 (Identity_Assertion,
                  Value (Iter).all.Attribute_Target_Supports);

               if Aux.Identity_Assertion then
                  Aux.Identity_Types :=
                    Value (Iter).all.Attribute_Identity_Types;

                  if Is_Set (ITT_Principal_Name, Aux.Identity_Types) then
                     Aux.Naming_Mechanisms :=
                       PolyORB.Security.Types.Duplicate
                       (Value (Iter).all.Attribute_Naming_Mechanisms);

                     if Aux.Naming_Mechanisms = OID_Lists.Empty then
                        Supported := False;
                     end if;
                  end if;

               else
                  Aux.Identity_Types := 0;
               end if;

               --  Privilege Authorities and Delegation By Client

               declare
                  AP_Iter : Service_Configuration_Lists.Iterator :=
                              First (Value (Iter).all.
                                       Attribute_Privilege_Authorities);
                  Aux_PA  : Client_Authority_Mechanism_Access;

               begin
                  while not Last (AP_Iter) loop
                     Aux_PA :=
                       Create_Client_Authority_Mechanism
                       (Value (AP_Iter).Syntax,
                        Value (AP_Iter).Name.all);

                     if Aux_PA = null then
                        Supported := False;

                     else
                        Append (Aux.Authorities, Aux_PA);
                     end if;

                     Next (AP_Iter);
                  end loop;
               end;

               if Is_Set
                 (Delegation_By_Client,
                  Value (Iter).all.Attribute_Target_Supports)
                 and then Value (Iter).all.Attribute_Privilege_Authorities
                   /= Service_Configuration_Lists.Empty
               then
                  Aux.Delegation_Supported := True;
                  Aux.Delegation_Required  :=
                    Is_Set
                    (Delegation_By_Client,
                     Value (Iter).all.Attribute_Target_Requires);

               else
                  Aux.Delegation_Supported := False;
                  Aux.Delegation_Required  := False;
               end if;

               Note.Mechanism := Value (Iter).all;
               Set_Note (Aux.Notepad, Note);

               --  If at least one layer mechanisms not supported, then
               --  compound mechanism not added to the list of QoS compound
               --  mechanisms

               if Supported then
                  Append (QoS.Mechanisms, Aux);

               else
                  Destroy (Aux);
               end if;

               Next (Iter);
            end;
         end loop;

         Set_Profile_QoS (P, Compound_Security, QoS_Parameter_Access (QoS));
      end;
   end Fetch_QoS;

   function Fetch_QoS
     (End_Point : PolyORB.Transport.Transport_Endpoint_Access)
      return PolyORB.QoS.QoS_Parameter_Access
   is
   begin
      if QoS_Registry /= null then
         return QoS_Registry (End_Point);

      else
         return null;
      end if;
   end Fetch_QoS;

   ----------------------------
   -- Fetch_Tagged_Component --
   ----------------------------

   function Fetch_Tagged_Component
     (Oid : PolyORB.Objects.Object_Id)
      return Tagged_Component_Access
   is
      QoS   : QoS_Parameters;
      Error : PolyORB.Errors.Error_Container;

   begin
      PolyORB.Obj_Adapters.Get_QoS
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB),
         Oid,
         QoS,
         Error);

      if PolyORB.Errors.Found (Error) then
         raise Program_Error;
      end if;

      if QoS (Compound_Security) /= null then
         declare
            Result : constant TC_CSI_Sec_Mech_List_Access
              := new TC_CSI_Sec_Mech_List;
            M      : constant Mechanism_Access := new Mechanism;
            Iter   : Target_Mechanism_Lists.Iterator
              := First
              (QoS_Target_Security_Parameter
               (QoS (Compound_Security).all).Mechanisms);

         begin
            Result.Stateful :=
              QoS_Target_Security_Parameter
              (QoS (Compound_Security).all).Stateful;

            while not Last (Iter) loop
               M.Target_Requires := Target_Requires (Value (Iter).all.all);

               --  Transport mechanism

               if Value (Iter).all.Transport = null then
                  M.Transport_Mechanism_Tag := new Null_Tag.TC_Null_Tag;

               else
                  declare
                     use Registry_Lists;

                     C_Iter : Registry_Lists.Iterator := First (Registry);

                  begin
                     while not Last (C_Iter) loop
                        M.Transport_Mechanism_Tag :=
                          Value (C_Iter).all.TC_Constructor
                          (Value (Iter).all.Transport);

                        exit when M.Transport_Mechanism_Tag /= null;

                        Next (C_Iter);
                     end loop;
                  end;
               end if;

               --  Authentication layer

               M.Authentication_Target_Supports := 0;
               M.Authentication_Target_Requires := 0;
               M.Authentication_Mechanism       := Null_Object_Identifier;
               M.Authentication_Target_Name     := null;

               if Value (Iter).all.Authentication_Mechanism /= null then
                  M.Authentication_Target_Supports :=
                    Establish_Trust_In_Client;

                  if  Value (Iter).all.Authentication_Required then
                     M.Authentication_Target_Requires :=
                       Establish_Trust_In_Client;
                  end if;

                  M.Authentication_Mechanism :=
                    Duplicate
                    (Get_Mechanism_OID
                     (Value (Iter).all.Authentication_Mechanism));
                  M.Authentication_Target_Name :=
                    Duplicate
                    (Get_Target_Name
                     (Value (Iter).all.Authentication_Mechanism));
               end if;

               --  Attribute layer

               M.Attribute_Target_Supports := 0;
               M.Attribute_Target_Requires := 0;
               M.Attribute_Identity_Types  := 0;

               if Value (Iter).all.Backward_Trust_Evaluator /= null
                 or else Value (Iter).all.Forward_Trust_Evaluator /= null
               then
                  M.Attribute_Target_Supports := Identity_Assertion;

                  M.Attribute_Naming_Mechanisms :=
                    PolyORB.Security.Types.Duplicate
                    (Value (Iter).all.Naming_Mechanisms);
                  M.Attribute_Identity_Types :=
                    Value (Iter).all.Identity_Types;
               end if;

               if Value (Iter).all.Authorities
                    /= Target_Authority_Mechanism_Lists.Empty
               then
                  declare
                     PA_Iter : Target_Authority_Mechanism_Lists.Iterator
                       := First (Value (Iter).all.Authorities);

                  begin
                     while not Last (PA_Iter) loop
                        Append
                          (M.Attribute_Privilege_Authorities,
                           (Get_Service_Configuration_Syntax
                            (Value (PA_Iter).all),
                            new Ada.Streams.Stream_Element_Array'
                            (Encode (Value (PA_Iter).all))));
                        Next (PA_Iter);
                     end loop;
                  end;

                  if Value (Iter).all.Forward_Trust_Evaluator /= null then
                     --  XXX DelegationByClient is a property of trust
                     --  evaluator! Should be investigated!

                     M.Attribute_Target_Supports :=
                       M.Attribute_Target_Supports or Delegation_By_Client;

                     if Value (Iter).all.Delegation_Required then
                        M.Attribute_Target_Requires := Delegation_By_Client;
                     end if;
                  end if;
               end if;

               Append (Result.Mechanisms, M);

               Next (Iter);
            end loop;

            return Tagged_Component_Access (Result);
         end;
      end if;

      return null;
   end Fetch_Tagged_Component;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Register Tagged Component

      Register (Tag_CSI_Sec_Mech_List, Create_Empty_Component'Access, null);

      --  Register Tagged Component => Transport Mechanisms convertor

      Register (Tag_CSI_Sec_Mech_List, Create_Transport_Mechanisms'Access);

      --  Register POA QoS => Tagged Component convertor

      PolyORB.Binding_Data.GIOP.IIOP.Security_Fetch_Tagged_Component :=
        Fetch_Tagged_Component'Access;

      --  Register Tagged Component => QoS convertor

      PolyORB.Binding_Data.GIOP.IIOP.Security_Fetch_QoS := Fetch_QoS'Access;

      --  Setup GIOP transport mechanism selection hook

      PolyORB.Binding_Data.GIOP.Is_Security_Selected := Is_Selected'Access;

      --  Setup GIOP secure transport => QoS hook

      PolyORB.Protocols.GIOP.Fetch_Secure_Transport_QoS := Fetch_QoS'Access;
   end Initialize;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (QoS       : PolyORB.QoS.QoS_Parameters;
      Mechanism :
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Access)
     return Boolean
   is
      Note : Compound_Mechanism_Note;

   begin
      if QoS (Transport_Security) = null then
         --  Unprotected invocation

         return True;
      end if;

      Get_Note
        (QoS_Transport_Context_Parameter_Access
         (QoS (Transport_Security)).Selected.Notepad,
         Note);

      return Mechanism = Note.Mechanism.Transport_Mechanism;
   end Is_Selected;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Item   :        Mechanism)
   is
   begin
      --  Marshall target requirements

      Marshall (Buffer, Unsigned_Short (Item.Target_Requires));

      --  Marshall transport mechanism tag

      Marshall_Tagged_Component (Buffer, Item.Transport_Mechanism_Tag);

      --  Marshall authentication layer configuration

      Marshall (Buffer, Unsigned_Short (Item.Authentication_Target_Supports));
      Marshall (Buffer, Unsigned_Short (Item.Authentication_Target_Requires));

      if Item.Authentication_Mechanism /= Null_Object_Identifier then
         Marshall (Buffer, Encode (Item.Authentication_Mechanism));

      else
         Marshall (Buffer, Stream_Element_Array'(1 .. 0 => 0));
      end if;

      if Item.Authentication_Target_Name /= null then
         Marshall (Buffer, Encode (Item.Authentication_Target_Name));

      else
         Marshall (Buffer, Stream_Element_Array'(1 .. 0 => 0));
      end if;

      --  Marshall attribute layer configuration

      Marshall (Buffer, Unsigned_Short (Item.Attribute_Target_Supports));
      Marshall (Buffer, Unsigned_Short (Item.Attribute_Target_Requires));

      Marshall
        (Buffer,
         Unsigned_Long (Length (Item.Attribute_Privilege_Authorities)));

      declare
         Iter : Service_Configuration_Lists.Iterator
           := First (Item.Attribute_Privilege_Authorities);

      begin
         while not Last (Iter) loop
            Marshall
              (Buffer,
               Unsigned_Long (Value (Iter).Syntax));
            Marshall (Buffer, Value (Iter).Name.all);
            Next (Iter);
         end loop;
      end;

      Marshall
        (Buffer, Unsigned_Long (Length (Item.Attribute_Naming_Mechanisms)));

      declare
         Iter : OID_Lists.Iterator := First (Item.Attribute_Naming_Mechanisms);

      begin
         while not Last (Iter) loop
            Marshall (Buffer, Encode (Value (Iter).all));
            Next (Iter);
         end loop;
      end;

      Marshall (Buffer, Unsigned_Long (Item.Attribute_Identity_Types));
   end Marshall;

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   overriding procedure Marshall_Component_Data
     (C      : access TC_CSI_Sec_Mech_List;
      Buffer : access Buffer_Type)
   is
      Temp_Buf : Buffer_Access := new Buffer_Type;

   begin
      --  The body of Tag_CSI_Sec_Mech_List component is an encapsulation

      Start_Encapsulation (Temp_Buf);

      Marshall (Temp_Buf, C.Stateful);

      Marshall (Temp_Buf, Unsigned_Long (Length (C.Mechanisms)));

      declare
         Iter : Mechanism_Lists.Iterator := First (C.Mechanisms);

      begin
         while not Last (Iter) loop
            Marshall (Temp_Buf, Value (Iter).all.all);
            Next (Iter);
         end loop;
      end;

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag            : Tag_Value;
      TC_Constructor : To_Tagged_Component;
      TM_Constructor : To_Security_Transport_Mechanism)
   is
   begin
      Registry_Lists.Append (Registry, (Tag, TC_Constructor, TM_Constructor));
   end Register;

   procedure Register (Constructor : QoS_Constructor) is
   begin
      QoS_Registry := Constructor;
   end Register;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (X : in out Mechanism) is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Tagged_Component'Class,


         Name   => Tagged_Component_Access);

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Stream_Element_Array,


         Name   => Stream_Element_Array_Access);

   begin
      Release_Contents (X.Transport_Mechanism_Tag);
      Free (X.Transport_Mechanism_Tag);

      Destroy (X.Authentication_Mechanism);
      Destroy (X.Authentication_Target_Name);

      declare
         Iter : Service_Configuration_Lists.Iterator
           := First (X.Attribute_Privilege_Authorities);

      begin
         while not Last (Iter) loop
            Free (Value (Iter).all.Name);
            Next (Iter);
         end loop;

         Deallocate (X.Attribute_Privilege_Authorities);
      end;

      declare
         Iter : OID_Lists.Iterator := First (X.Attribute_Naming_Mechanisms);

      begin
         while not Last (Iter) loop
            Destroy (Value (Iter).all);
            Next (Iter);
         end loop;

         Deallocate (X.Attribute_Naming_Mechanisms);
      end;
   end Release_Contents;

   overriding procedure Release_Contents (C : access TC_CSI_Sec_Mech_List) is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Mechanism,


         Name   => Mechanism_Access);

      Iter : Mechanism_Lists.Iterator := First (C.Mechanisms);

   begin
      while not Last (Iter) loop
         Release_Contents (Value (Iter).all.all);
         Free (Value (Iter).all);
         Next (Iter);
      end loop;

      Deallocate (C.Mechanisms);
   end Release_Contents;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type) return Mechanism is
      Result : Mechanism;
      Error  : Error_Container;

   begin
      --  Target requirements

      Result.Target_Requires :=
        Association_Options (Unsigned_Short'(Unmarshall (Buffer)));

      --  Transport mechanism tag

      Unmarshall_Tagged_Component
        (Buffer,
         Result.Transport_Mechanism_Tag,
         Error);

      pragma Assert (not Found (Error));
      --  XXX Should be properly handled.

      --  Authentication layer configuration

      Result.Authentication_Target_Supports :=
        Association_Options (Unsigned_Short'(Unmarshall (Buffer)));
      Result.Authentication_Target_Requires :=
        Association_Options (Unsigned_Short'(Unmarshall (Buffer)));

      declare
         Aux : constant Stream_Element_Array := Unmarshall (Buffer);

      begin
         if Aux'Length /= 0 then
            Result.Authentication_Mechanism := Decode (Aux);

         else
            Result.Authentication_Mechanism := Null_Object_Identifier;
         end if;
      end;

      declare
         Aux : constant Stream_Element_Array := Unmarshall (Buffer);

      begin
         if Aux'Length /= 0 then
            Decode (Aux, Result.Authentication_Target_Name, Error);

            if Found (Error) then
               Catch (Error);

               raise Program_Error;
            end if;

         else
            Result.Authentication_Target_Name := null;
         end if;
      end;

      --  Attribute layer configuration

      Result.Attribute_Target_Supports :=
        Association_Options (Unsigned_Short'(Unmarshall (Buffer)));
      Result.Attribute_Target_Requires :=
        Association_Options (Unsigned_Short'(Unmarshall (Buffer)));

      declare
         Length : constant Unsigned_Long := Unmarshall (Buffer);

      begin
         for J in 1 .. Length loop
            declare
               Syntax : constant Service_Configuration_Syntax
                 := Service_Configuration_Syntax
                 (Unsigned_Long'(Unmarshall (Buffer)));

            begin
               Append
                 (Result.Attribute_Privilege_Authorities,
                  (Syntax, new Stream_Element_Array'(Unmarshall (Buffer))));
            end;
         end loop;
      end;

      declare
         Length : constant Unsigned_Long := Unmarshall (Buffer);

      begin
         for J in 1 .. Length loop
            Append
             (Result.Attribute_Naming_Mechanisms,
              Decode (Unmarshall (Buffer)));
         end loop;
      end;

      Result.Attribute_Identity_Types :=
        Identity_Token_Type (Unsigned_Long'(Unmarshall (Buffer)));

      return Result;
   end Unmarshall;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_CSI_Sec_Mech_List;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);
      Temp_Buf : Buffer_Access         := new Buffer_Type;

      Len : Unsigned_Long;

   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);

      C.Stateful := Unmarshall (Temp_Buf);

      Len := Unmarshall (Temp_Buf);

      for J in 1 .. Len loop
         Append (C.Mechanisms, new Mechanism'(Unmarshall (Temp_Buf)));
      end loop;

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
         Release (Temp_Buf);
         Throw (Error,
                Bad_Param_E,
                System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name     => +"giop_p.tagged_components.csi_sec_mech_list",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => PolyORB.Initialization.String_Lists.Empty,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
