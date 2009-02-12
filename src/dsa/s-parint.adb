------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2009, Free Software Foundation, Inc.          --
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
with Ada.Unchecked_Conversion;

with System.Address_To_Access_Conversions;

with GNAT.HTable;

with PolyORB.Binding_Data;
with PolyORB.DSA_P.Exceptions;
with PolyORB.Dynamic_Dict;
with PolyORB.Errors;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters;
pragma Elaborate_All (PolyORB.Parameters);
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.POA_Config.RACWs;
with PolyORB.POA_Manager;
with PolyORB.POA_Types;
with PolyORB.QoS;
with PolyORB.QoS.Exception_Informations;
with PolyORB.QoS.Term_Manager_Info;
with PolyORB.References.Binding;
with PolyORB.Request_QoS;
with PolyORB.Sequences.Unbounded;
with PolyORB.Sequences.Unbounded.Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.Helper);
with PolyORB.Servants.Iface;
with PolyORB.Services.Naming;
with PolyORB.Services.Naming.Helper;
with PolyORB.Services.Naming.NamingContext;
with PolyORB.Services.Naming.NamingContext.Client;
with PolyORB.Setup;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Termination_Activity;
with PolyORB.Utils.Strings.Lists;

package body System.Partition_Interface is

   use Ada.Characters.Handling;
   use Ada.Streams;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.References;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("system.partition_interface");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --  A few handy aliases

   package PATC  renames PolyORB.Any.TypeCode;
   package PSNNC renames PolyORB.Services.Naming.NamingContext;
   package PTC   renames PolyORB.Tasking.Condition_Variables;
   package PTM   renames PolyORB.Tasking.Mutexes;

   --  An opaque octet sequence

   package Octet_Sequences is
     new PolyORB.Sequences.Unbounded (PolyORB.Types.Octet);

   package Octet_Sequences_Helper is new Octet_Sequences.Helper
     (Element_From_Any => PolyORB.Any.From_Any,
      Element_To_Any   => PolyORB.Any.To_Any,
      Element_Wrap     => PolyORB.Any.Wrap);

   TC_Opaque_Cache : PATC.Local_Ref;
   --  Typecode for the opaque octet sequence

   ---------------------------------------------------------------
   -- Special operation names for remote call interface objects --
   ---------------------------------------------------------------

   Op_Resolve : constant String := "resolve";
   --  Corresponds to the CORBA CosNaming::NamingContext::resolve operation.

   Op_Get_Partition_Id : constant String := "_get_partition_id";
   --  Get the DSA partition identifier for the partition that hosts an RCI

   ------------------------
   -- Local declarations --
   ------------------------

   Critical_Section : PTM.Mutex_Access;
   --  Protects shared data structures at the DSA personality level

   procedure Initialize;
   --  Procedure called during global PolyORB initialization

   function Is_Reference_Valid (R : PolyORB.References.Ref) return Boolean;
   --  Binds a reference to determine whether it is valid

   procedure Detach;
   --  Detach a procedure using setsid() and closing the standard
   --  input/standard output/standard error file descriptors.

   ------------------------------------------------
   -- Termination manager of the local partition --
   ------------------------------------------------

   --  These values are set by Register_Termination_Manager, which is called
   --  during elaboration of Termination_Manager.Bootstrap.

   The_TM_Ref : Ref := Nil_Ref;
   --  Reference to the termination manager

   The_TM_Oid     : PolyORB.Objects.Object_Id_Access;
   --  The termination manager Object ID

   The_TM_Address : System.Address;
   --  The local termination manager servant address

   The_TM_Shutdown : PolyORB.Initialization.Finalizer;
   --  The local termination manager shutdown hook

   --------------------------------
   -- Map of all known RCI units --
   --------------------------------

   type RCI_Info is record

      Is_All_Calls_Remote : Boolean := True;
      --  True if the package is remote or pragma All_Call_Remotes applies

      Base_Ref            : Object_Ref;
      --  Main reference for package

      Is_Local            : Boolean := False;
      --  True if the package is assigned on local partition

      Known_Partition_ID  : Boolean := False;
      --  True if the package is not assigned on local partition, and its
      --  partition ID is known.

      RCI_Partition_ID    : RPC.Partition_ID := RPC.Partition_ID'First;
      --  Cache of RCI's partition ID, if known

   end record;

   package Known_RCIs is new PolyORB.Dynamic_Dict (RCI_Info);
   --  This list is keyed with the lowercased full names of the RCI units

   function Retrieve_RCI_Info (Name : String) return RCI_Info;
   --  Retrieve RCI information for a local or remote RCI package

   --  To limit the amount of memory leaked by the use of distributed object
   --  stub types, these are referenced in a hash table and reused whenever
   --  possible. Access to this hash table is protected by the DSA critical
   --  section.

   type Hash_Index is range 0 .. 100;
   function Hash (K : RACW_Stub_Type_Access) return Hash_Index;

   function Compare_Content (Left, Right : RACW_Stub_Type_Access)
     return Boolean;

   package Objects_HTable is
      new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Index,
      Element    => RACW_Stub_Type_Access,
      No_Element => null,
      Key        => RACW_Stub_Type_Access,
      Hash       => Hash,
      Equal      => Compare_Content);

   --  When a RACW must be constructed to designate a local object, an object
   --  identifier is created using the address of the object.

   subtype Local_Oid is PolyORB.Objects.Object_Id
     (1 .. System.Address'Size / 8);

   function To_Local_Oid is
     new Ada.Unchecked_Conversion (System.Address, Local_Oid);
   function To_Address is
     new Ada.Unchecked_Conversion (Local_Oid, System.Address);

   procedure Setup_Object_RPC_Receiver
     (Name            : String;
      Default_Servant : Servant_Access);
   --  Set up an object adapter to receive method invocation requests for
   --  distributed object type Name. Use the specified POA configuration (which
   --  must include the USER_ID, NON_RETAIN and USE_DEFAULT_SERVANT policies).
   --  The components of Servant are set appropriately.

   function DSA_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return Any;
   --  Construct an Any from an Ada exception raised by a servant

   function To_Name (Id, Kind : String) return PolyORB.Services.Naming.Name;
   --  Construct a name consisting of a single name component with the given
   --  id and kind.

   function Naming_Context return PSNNC.Ref;
   --  Naming context used to register all library units in a DSA application

   Naming_Context_Cache : PSNNC.Ref;

   --  End of local declarations

   ---------------------
   -- Allocate_Buffer --
   ---------------------

   procedure Allocate_Buffer (Stream : in out Buffer_Stream_Type) is
      use type PolyORB.Buffers.Buffer_Access;
   begin
      pragma Assert (Stream.Buf = null);
      Stream.Buf := new PolyORB.Buffers.Buffer_Type;
   end Allocate_Buffer;

   -------------------------
   -- Any_Aggregate_Build --
   -------------------------

   function Any_Aggregate_Build
     (TypeCode : PATC.Local_Ref;
      Contents : Any_Array) return Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TypeCode);
   begin
      for J in Contents'Range loop
         Add_Aggregate_Element (Result, Contents (J));
      end loop;
      return Result;
   end Any_Aggregate_Build;

   ---------------------
   -- Any_Member_Type --
   ---------------------

   function Any_Member_Type
     (A     : Any;
      Index : System.Unsigned_Types.Long_Unsigned)
     return PolyORB.Any.TypeCode.Local_Ref
   is
   begin
      return PATC.Member_Type
        (PolyORB.Any.Get_Type (A), PolyORB.Types.Unsigned_Long (Index));
   end Any_Member_Type;

   ---------------
   -- Any_To_BS --
   ---------------

   procedure Any_To_BS (Item : Any; Stream : out Buffer_Stream_Type) is
      use Octet_Sequences;
      Seq : constant Sequence := Octet_Sequences_Helper.From_Any (Item);
   begin
      Stream.Arr := new Stream_Element_Array'
        (1 .. Stream_Element_Offset (Length (Seq)) => 0);

      declare
         subtype OSEA_T is Element_Array (1 .. Length (Seq));
         OSEA_Addr : constant System.Address := Stream.Arr (1)'Address;
         OSEA : OSEA_T;
         for OSEA'Address use OSEA_Addr;
         pragma Import (Ada, OSEA);
      begin
         OSEA := To_Element_Array (Seq);
      end;

      PolyORB.Buffers.Initialize_Buffer
        (Stream.Buf,
         Stream.Arr'Length,
         Stream.Arr (Stream.Arr'First)'Address,
         PolyORB.Buffers.Endianness_Type'First, --  XXX Irrelevant
         0);
   end Any_To_BS;

   ---------------
   -- BS_To_Any --
   ---------------

   procedure BS_To_Any (Stream : Buffer_Stream_Type; Item : out Any) is
      use Octet_Sequences;

      S : PolyORB.Opaque.Zone_Access :=
            new Stream_Element_Array'(PolyORB.Buffers.To_Stream_Element_Array
                                        (Stream.Buf));

      subtype OSEA_T is Element_Array (1 .. S'Length);
      OSEA_Addr : constant System.Address := S (S'First)'Address;
      OSEA : OSEA_T;
      for OSEA'Address use OSEA_Addr;
      pragma Import (Ada, OSEA);
   begin
      Item := Octet_Sequences_Helper.To_Any (To_Sequence (OSEA));
      PolyORB.Opaque.Free (S);
   end BS_To_Any;

   ---------------------------
   -- Build_Local_Reference --
   ---------------------------

   procedure Build_Local_Reference
     (Addr     : System.Address;
      Typ      : String;
      Receiver : access Servant;
      Ref      : out PolyORB.References.Ref)
   is
      use PolyORB.Errors;
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;

      Last : Integer := Typ'Last;

      Error : Error_Container;
   begin
      if Last in Typ'Range and then Typ (Last) = ASCII.NUL then
         Last := Last - 1;
      end if;

      if Addr /= Null_Address then
         pragma Assert (Receiver.Object_Adapter /= null);

         declare
            Key : aliased PolyORB.Objects.Object_Id := To_Local_Oid (Addr);
            U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
            Oid : PolyORB.POA_Types.Object_Id_Access;

         begin
            PolyORB.POA.Activate_Object
              (Self      => PolyORB.POA.Obj_Adapter_Access
                              (Receiver.Object_Adapter),
               P_Servant => null,
               Hint      => Key'Unchecked_Access,
               U_Oid     => U_Oid,
               Error     => Error);

            if Found (Error) then
               PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
            end if;

            Oid := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);

            if Found (Error) then
               PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
            end if;

            PolyORB.ORB.Create_Reference
              (PolyORB.Setup.The_ORB,
               Oid, "DSA:" & Typ (Typ'First .. Last), Ref);

            PolyORB.Objects.Free (Oid);
         end;
      end if;
   exception
      when E : others =>
         pragma Debug
           (C, O ("Build_Local_Reference: got exception "
                 & Ada.Exceptions.Exception_Information (E)));
         pragma Debug (C, O ("returning a nil ref."));
         null;
   end Build_Local_Reference;

   ------------------------
   -- Caseless_String_Eq --
   ------------------------

   function Caseless_String_Eq (S1, S2 : String) return Boolean is
   begin
      if S1'Length /= S2'Length then
         return False;
      end if;

      for I in S1'Range loop
         if To_Lower (S1 (I)) /= To_Lower (S2 (I - S1'First + S2'First)) then
            return False;
         end if;
      end loop;

      return True;
   end Caseless_String_Eq;

   -----------
   -- Check --
   -----------

   procedure Check
     (Name    : String;
      Version : String;
      RCI     : Boolean := True)
   is
      use Ada.Exceptions;
   begin
      pragma Debug (C, O ("Check: checking RCI versions consistency"));

      if not RCI then
         return;
      end if;

      declare
         Info       : constant RCI_Info := Retrieve_RCI_Info (Name);
         Type_Id    : constant String   := Type_Id_Of (Info.Base_Ref);
         Last_Colon : Integer;
      begin

         for C in reverse Type_Id'Range loop
            if Type_Id (C) = ':' then
               Last_Colon := C;
               exit;
            end if;
         end loop;

         if Version /= Type_Id (Last_Colon + 1 .. Type_Id'Last) then
            raise Program_Error
              with "Versions differ for unit """ & Name & """";
         end if;
      end;
   end Check;

   ---------------------
   -- Compare_Content --
   ---------------------

   function Compare_Content
     (Left, Right : RACW_Stub_Type_Access) return Boolean
   is
      use System.RPC;
      Left_Object, Right_Object : PolyORB.References.Ref;
   begin
      Set (Left_Object, Left.Target);
      Set (Right_Object, Right.Target);

      return Left /= null and then Right /= null
        and then PolyORB.References.Is_Equivalent (Left_Object, Right_Object);
   end Compare_Content;

   --------------------------
   -- DSA_Exception_To_Any --
   --------------------------

   function DSA_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return Any
   is
      use PolyORB.Errors;
      use PolyORB.Types;

      Name   : constant String := PolyORB.Exceptions.Occurrence_To_Name (E);
      TC     : constant PATC.Local_Ref := PATC.TC_Except;
      Result : PolyORB.Any.Any;
   begin
      --  Name

      PATC.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));

      --  RepositoryId

      PATC.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                       (PolyORB.DSA_P.Exceptions.Exception_Repository_Id
                          (Name, "1.0"))));

      --  Valuation: Exception_Message

      PATC.Add_Parameter (TC, To_Any (TC_String));
      PATC.Add_Parameter (TC,
        To_Any (To_PolyORB_String ("exception_message")));

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element
        (Result, To_Any
           (To_PolyORB_String (Ada.Exceptions.Exception_Message (E))));
      return Result;
   end DSA_Exception_To_Any;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access Servant;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants.Iface;

      Result : PolyORB.Components.Null_Message;
   begin
      if Msg in Execute_Request then
         declare
            EMsg : Execute_Request renames Execute_Request (Msg);
         begin
            if Receiving_Stub (Self.Impl_Info.all).Kind = Pkg_Stub then

               --  The base reference for an RCI unit implements operations
               --  that correspond to the visible subprograms of the unit
               --  (which are handled by Self.Handler).
               --  In addition, it implements the following special operations:

               --  XXX these hand-crafted skels should be generated by
               --  auto-generated ones constructed from a distributed object
               --  type declaration.

               if EMsg.Req.Operation.all = Op_Resolve then

                  -------------
                  -- resolve --
                  -------------

                  --  Resolve the name of a remote subprogram declared in this
                  --  remote call interface unit to the corresponding reference
                  --  for the purpose of constructing a RAS value.

                  --  Code extracted from CosNaming::NamingContext IDL skel.

                  declare
                     package ISNC renames
                       PolyORB.Services.Naming.SEQUENCE_NameComponent;

                     n             : PolyORB.Services.Naming.Name;

                     Arg_Name_n  : constant PolyORB.Types.Identifier :=
                       To_PolyORB_String ("n");
                     Argument_n  : constant Any :=
                                     Get_Empty_Any (
                                       PolyORB.Services.Naming.Helper.TC_Name);

                     Result      : Object_Ref;
                     Arg_List    : NVList_Ref;
                  begin
                     --  Create argument list

                     NVList_Create (Arg_List);
                     NVList_Add_Item
                       (Arg_List,
                        Arg_Name_n,
                        Argument_n,
                        ARG_IN);

                     Request_Arguments (EMsg.Req, Arg_List);

                     --  Convert arguments from their Any

                     n := PolyORB.Services.Naming.Helper.From_Any (Argument_n);

                     --  Call implementation

                     Get_RAS_Info
                       (Receiving_Stub (Self.Impl_Info.all).Name.all,
                        PolyORB.Services.Naming.To_Standard_String
                          (ISNC.Get_Element (ISNC.Sequence (n), 1).id),
                           Result);

                     --  Set Result

                     EMsg.Req.Result :=
                       (Name      => PolyORB.Types.To_PolyORB_String
                        ("result"),
                        Arg_Modes => ARG_OUT,
                        Argument  => PolyORB.Any.ObjRef.To_Any (Result));
                  end;
                  goto Request_Completed;

               elsif EMsg.Req.Operation.all = Op_Get_Partition_Id then
                  declare
                     Arg_List    : NVList_Ref;
                  begin

                     -----------------------
                     -- _get_partition_id --
                     -----------------------

                     --  Return the partition identifier assigned to the
                     --  partition on which this RCI unit resides.

                     NVList_Create (Arg_List);
                     Request_Arguments (EMsg.Req, Arg_List);

                     --  Must call Arguments (with an empty Arg_List) to
                     --  notify the protocol personality that this request has
                     --  been completely received.

                     EMsg.Req.Result :=
                       (Name      => PolyORB.Types.To_PolyORB_String
                          ("result"),
                        Arg_Modes => ARG_OUT,
                        Argument  => TA_I (Integer (Get_Local_Partition_ID)));
                     goto Request_Completed;
                  end;
               end if;
            end if;

            --  User-defined subprogram: perform upcall to implementation

            --  Extract service context info used by the termination manager

            PolyORB.QoS.Term_Manager_Info.Extract_TM_Info (EMsg.Req);

            pragma Assert (Self.Handler /= null);
            declare
               use PolyORB.Errors;
            begin
               Self.Handler.all (EMsg.Req);
            exception
               when E : others =>
                  --  Save exception occurrence in request

                  EMsg.Req.Exception_Info := DSA_Exception_To_Any (E);

                  --  Also record additional exception information in optional
                  --  service context.

                  PolyORB.QoS.Exception_Informations.Set_Exception_Information
                    (EMsg.Req, E);
            end;

            <<Request_Completed>>
            return Executed_Request'(Req => EMsg.Req);
         end;
      end if;
      return Result;
   end Execute_Servant;

   -------------------------
   -- Extract_Union_Value --
   -------------------------

   function Extract_Union_Value (U : Any) return Any is
      U_Type : constant PATC.Local_Ref := Get_Type (U);
      Label_Any : constant Any
        := PolyORB.Any.Get_Aggregate_Element
        (U, PATC.Discriminator_Type (U_Type), 0);
      Value_Type : constant PATC.Local_Ref
        := PATC.Member_Type_With_Label (U_Type, Label_Any);
   begin
      return PolyORB.Any.Get_Aggregate_Element (U, Value_Type, 1);
   end Extract_Union_Value;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Buffer_Stream_Type) is
   begin
      PolyORB.Opaque.Free (X.Arr);
   end Finalize;

   --------------
   -- From_Any --
   --------------

   function FA_A (Item : PolyORB.Any.Any) return DSAT.Any_Container_Ptr is
      Item_ACP : constant PolyORB.Any.Any_Container_Ptr :=
                   PolyORB.Any.Get_Container (PolyORB.Any.From_Any (Item));
      pragma Warnings (Off);
      --  No aliasing issues since DSAT.Any_Container_Ptr values are never
      --  dereferenced without first being converted back to
      --  PolyORB.Any.Any_Container_Ptr.

      function To_DSAT_ACP is new Ada.Unchecked_Conversion
        (PolyORB.Any.Any_Container_Ptr, DSAT.Any_Container_Ptr);
   begin
      return To_DSAT_ACP (Item_ACP);
   end FA_A;

   function FA_B (Item : PolyORB.Any.Any) return Boolean is
   begin
      return Boolean (PolyORB.Types.Boolean'(From_Any (Item)));
   end FA_B;

   function FA_C (Item : PolyORB.Any.Any) return Character is
   begin
      return Character (PolyORB.Types.Char'(From_Any (Item)));
   end FA_C;

   function FA_F (Item : PolyORB.Any.Any) return Float is
   begin
      return Float (PolyORB.Types.Float'(From_Any (Item)));
   end FA_F;

   function FA_I (Item : PolyORB.Any.Any) return Integer is
   begin
      return Integer (PolyORB.Types.Long'(From_Any (Item)));
   end FA_I;

   function FA_U (Item : PolyORB.Any.Any) return Unsigned is
   begin
      return Unsigned (PolyORB.Types.Unsigned_Long'(From_Any (Item)));
   end FA_U;

   function FA_LF (Item : PolyORB.Any.Any) return Long_Float is
   begin
      return Long_Float (PolyORB.Types.Double'(From_Any (Item)));
   end FA_LF;

   function FA_LI (Item : PolyORB.Any.Any) return Long_Integer is
   begin
      return Long_Integer (PolyORB.Types.Long'(From_Any (Item)));
   end FA_LI;

   function FA_LU (Item : PolyORB.Any.Any) return Long_Unsigned is
   begin
      return Long_Unsigned (PolyORB.Types.Unsigned_Long'(From_Any (Item)));
   end FA_LU;

   function FA_LLF (Item : PolyORB.Any.Any) return Long_Long_Float is
   begin
      return Long_Long_Float (PolyORB.Types.Long_Double'(From_Any (Item)));
   end FA_LLF;

   function FA_LLI (Item : PolyORB.Any.Any) return Long_Long_Integer is
   begin
      return Long_Long_Integer (PolyORB.Types.Long_Long'(From_Any (Item)));
   end FA_LLI;

   function FA_LLU (Item : PolyORB.Any.Any) return Long_Long_Unsigned is
   begin
      return Long_Long_Unsigned
        (PolyORB.Types.Unsigned_Long_Long'(From_Any (Item)));
   end FA_LLU;

   function FA_SF (Item : PolyORB.Any.Any) return Short_Float is
   begin
      return Short_Float (PolyORB.Types.Float'(From_Any (Item)));
   end FA_SF;

   function FA_SI (Item : PolyORB.Any.Any) return Short_Integer is
   begin
      return Short_Integer (PolyORB.Types.Short'(From_Any (Item)));
   end FA_SI;

   function FA_SU (Item : PolyORB.Any.Any) return Short_Unsigned is
   begin
      return Short_Unsigned (PolyORB.Types.Short'(From_Any (Item)));
   end FA_SU;

   function FA_SSI (Item : PolyORB.Any.Any) return Short_Short_Integer is
      function To_SSI is new Ada.Unchecked_Conversion
        (PolyORB.Types.Octet, Short_Short_Integer);
   begin
      return To_SSI (From_Any (Item));
   end FA_SSI;

   function FA_SSU (Item : PolyORB.Any.Any) return Short_Short_Unsigned is
      function To_SSU is new Ada.Unchecked_Conversion
        (PolyORB.Types.Octet, Short_Short_Unsigned);
   begin
      return To_SSU (From_Any (Item));
   end FA_SSU;

   function FA_WC (Item : PolyORB.Any.Any) return Wide_Character is
   begin
      return Wide_Character (PolyORB.Types.Wchar'(From_Any (Item)));
   end FA_WC;

   function FA_String
     (Item : PolyORB.Any.Any) return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Ada.Strings.Unbounded.Unbounded_String
        (PolyORB.Types.String'(From_Any (Item)));
   end FA_String;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (Value : Any;
      Tc    : PATC.Local_Ref;
      Index : System.Unsigned_Types.Long_Unsigned)
      return Any is
   begin
      return PolyORB.Any.Get_Aggregate_Element
        (Value, Tc, PolyORB.Types.Unsigned_Long (Index));
   end Get_Aggregate_Element;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID (Name : Unit_Name) return RPC.Partition_ID
   is
      Is_Local : constant Boolean :=
        PolyORB.Parameters.Get_Conf ("dsa_local_rcis", To_Lower (Name));

      Info     : RCI_Info;
   begin

      --  If the unit is local, we should return the partition_id of the local
      --  partition.
      --  To determine if the unit is local, we cannot use the
      --  RCI_Info.Is_Local attribute, since RCI_Info will only be available
      --  after the RCI receiving stub is registered (and this is not
      --  guaranteed to happen before a call to Get_Active_Partition_Id is
      --  issued).
      --  Thus we use the configuration parameters set up by po_gnatdist in the
      --  per-partition specific unit 'PolyORB.Parameters.Partition'.

      if Is_Local then
         return Get_Local_Partition_ID;
      end if;

      Info := Retrieve_RCI_Info (Name);

      if not Info.Known_Partition_ID then
         declare
            Request : PolyORB.Requests.Request_Access;
            Arg_List : PolyORB.Any.NVList.Ref;
            Result : PolyORB.Any.NamedValue;
         begin

            --  XXX This hand-crafted stub should be replaced with
            --  one automatically generated from a remote object type
            --  declaration.

            PolyORB.Any.NVList.Create (Arg_List);
            Result := (Name => To_PolyORB_String ("result"),
                       Argument => Get_Empty_Any (TC_I),
                       Arg_Modes => 0);

            PolyORB.Requests.Create_Request
              (Target    => Info.Base_Ref,
               Operation => Op_Get_Partition_Id,
               Arg_List  => Arg_List,
               Result    => Result,
               Req       => Request);

            PolyORB.Requests.Invoke (Request);
            PolyORB.Requests.Destroy_Request (Request);
            Info.Known_Partition_ID := True;
            Info.RCI_Partition_ID   :=
              RPC.Partition_ID (FA_I (Result.Argument));
            Known_RCIs.Register (To_Lower (Name), Info);
         end;
      end if;
      pragma Assert (Info.Known_Partition_ID);
      return Info.RCI_Partition_ID;
   end Get_Active_Partition_ID;

   -----------------------
   -- Get_Local_Address --
   -----------------------

   procedure Get_Local_Address
     (Ref      : PolyORB.References.Ref;
      Is_Local : out Boolean;
      Addr     : out System.Address)
   is
      use PolyORB.Errors;

      Profiles : constant Profile_Array
        := PolyORB.References.Profiles_Of (Ref);

      Error : Error_Container;

   begin
      for J in Profiles'Range loop
         if PolyORB.ORB.Is_Profile_Local
           (PolyORB.Setup.The_ORB,
            Profiles (J))
         then
            declare
               use PolyORB.Binding_Data;
               use PolyORB.Objects;
               Key : Object_Id_Access;
            begin
               PolyORB.Obj_Adapters.Object_Key
                 (OA      =>  PolyORB.ORB.Object_Adapter
                  (PolyORB.Setup.The_ORB),
                  Id      => PolyORB.Binding_Data.Get_Object_Key
                  (Profiles (J).all),
                  User_Id => Key,
                  Error   => Error);

               if not Found (Error) then
                  Is_Local := True;

                  if The_TM_Oid /= null and then
                    Get_Object_Key (Profiles (J).all).all = The_TM_Oid.all
                  then
                     --  Requests for the Termination Manager do not contain
                     --  the local memory address of the object (they are
                     --  performed through a reference created under a specific
                     --  POA and a dummy object key) so these have to be
                     --  handled specifically.

                     Addr := The_TM_Address;
                  else

                     Addr := To_Address (Key (Key'Range));
                  end if;

                  PolyORB.Objects.Free (Key);
                  --  XXX not sure we can do that ...

                  return;
               elsif Error.Kind = Invalid_Object_Id_E then
                  Catch (Error);

                  --  This object identifier does not contain a user-assigned
                  --  object key.

               else
                  PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
               end if;
            end;
         end if;
      end loop;

      Is_Local := False;
      Addr := Null_Address;
   end Get_Local_Address;

   ------------
   -- Get_TC --
   ------------

   function Get_TC (A : Any) return PolyORB.Any.TypeCode.Local_Ref is
   begin
      return PATC.To_Ref (PolyORB.Any.Get_Unwound_Type (A));
   end Get_TC;

   Local_PID_Barrier   : PTC.Condition_Access;
   Local_PID           : RPC.Partition_ID;
   Local_PID_Allocated : Boolean := False;

   ----------------------------
   -- Set_Local_Partition_ID --
   ----------------------------

   procedure Set_Local_Partition_ID (PID : RPC.Partition_ID) is
   begin
      PTM.Enter (Critical_Section);
      if not Local_PID_Allocated then
         Local_PID := PID;
         Local_PID_Allocated := True;
         PTC.Broadcast (Local_PID_Barrier);
      end if;
      PTM.Leave (Critical_Section);
   end Set_Local_Partition_ID;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID return RPC.Partition_ID is
   begin
      PTM.Enter (Critical_Section);
      if not Local_PID_Allocated then

         --  Wait until a partition identifier has been assigned to the
         --  local partition (this barrier is opened once System.DSA_Services
         --  is elaborated).

         PTC.Wait (Local_PID_Barrier, Critical_Section);
         pragma Assert (Local_PID_Allocated);
      end if;
      PTM.Leave (Critical_Section);

      return Local_PID;
   end Get_Local_Partition_ID;

   --------------------------------
   -- Get_Nested_Sequence_Length --
   --------------------------------

   function Get_Nested_Sequence_Length
     (Value : Any;
      Depth : Positive)
     return Unsigned
   is
      use type PolyORB.Types.Unsigned_Long;

      Seq_Any : PolyORB.Any.Any;
      Tc      : constant PATC.Local_Ref := Get_Type (Value);
   begin
      pragma Debug (C, O ("Get_Nested_Sequence_Length: enter,"
                       & " Depth =" & Depth'Img & ","
                       & " Tc = " & Image (Tc)));

      if PATC.Kind (Tc) = Tk_Struct then
         declare
            Index : constant PolyORB.Types.Unsigned_Long
              := PATC.Member_Count (Tc) - 1;
         begin
            pragma Debug
              (C, O ("Index of last member is" & Index'Img));

            Seq_Any := Get_Aggregate_Element (Value,
              PATC.Member_Type (Tc, Index),
              Index);
         end;
      else
         pragma Debug (C, O ("Tc is (assumed to be) a Tk_Sequence"));
         Seq_Any := Value;
      end if;

      declare
         use type Unsigned;

         Outer_Length : constant Unsigned
           := FA_U (PolyORB.Any.Get_Aggregate_Element (Seq_Any, TC_U, 0));
      begin
         if Depth = 1 or else Outer_Length = 0 then
            return Outer_Length;
         else
            Seq_Any := PolyORB.Any.Get_Aggregate_Element
                         (Seq_Any, Content_Type (Get_Type (Seq_Any)), 1);
            return Get_Nested_Sequence_Length (Seq_Any, Depth - 1);
         end if;
      end;
   end Get_Nested_Sequence_Length;

   --------------
   -- Get_RACW --
   --------------

   function Get_RACW
     (Ref              : PolyORB.References.Ref;
      Stub_Tag         : Ada.Tags.Tag;
      Is_RAS           : Boolean;
      Asynchronous     : Boolean) return System.Address
   is
      Is_Local     : Boolean;
      Addr         : System.Address;

      Stub_Obj     : aliased RACW_Stub_Type;
      Stub_Acc     : RACW_Stub_Type_Access := Stub_Obj'Unchecked_Access;

      Stub_Obj_Tag : access Ada.Tags.Tag;

   begin
      --  Case of a nil reference: return a null address

      if Is_Nil (Ref) then
         return Null_Address;
      end if;

      Get_Local_Address (Ref, Is_Local, Addr);

      --  Local case: return address of local object

      if Is_Local then
         declare
            RAS_Proxy : RAS_Proxy_Type;
            for RAS_Proxy'Address use Addr;
            pragma Import (Ada, RAS_Proxy);
         begin
            if not (Is_RAS and then RAS_Proxy.All_Calls_Remote) then
               return Addr;
            end if;
         end;
      end if;

      --  Remote case: return address of stub

      Stub_Obj.Target := Entity_Of (Ref);
      Inc_Usage (Stub_Obj.Target);

      Stub_Obj.Asynchronous := Asynchronous;

      Get_Unique_Remote_Pointer (Stub_Acc);

      --  Fix up stub tag. This is safe because we carefully ensure that
      --  all stub types have the same layout as RACW_Stub_Type.

      declare
         CW_Stub_Obj : RACW_Stub_Type'Class
                         renames RACW_Stub_Type'Class (Stub_Acc.all);
         --  Class-wide view of stub object, to which 'Tag can be applied
      begin
         Stub_Obj_Tag := CW_Stub_Obj'Tag'Unrestricted_Access;
         Stub_Obj_Tag.all := Stub_Tag;
      end;

      return Stub_Acc.all'Address;
   end Get_RACW;

   ------------------
   -- Get_RAS_Info --
   ------------------

   procedure Get_RAS_Info
     (Pkg_Name        :     String;
      Subprogram_Name :     String;
      Subp_Ref        : out Object_Ref)
   is
      Info : constant RCI_Info := Retrieve_RCI_Info (Pkg_Name);

   begin
      if Info.Is_Local then
         --  Retrieve subprogram address using subprogram name and subprogram
         --  table. Warning: the name used MUST be the distribution-name (with
         --  overload suffix, where appropriate.)

         declare
            use Receiving_Stub_Lists;
            It : Receiving_Stub_Lists.Iterator :=
              First (All_Receiving_Stubs);

            Addr : System.Address := System.Null_Address;
            Receiver : Servant_Access := null;

         begin

            --  XXX
            --  The following is ugly and inefficient (two levels of linear
            --  search) and should probably be optimized in some way.

            pragma Debug (C, O ("Looking up RAS ref for " &
                               Subprogram_Name & " in " &
                               Pkg_Name));

            All_Stubs :
            while not Last (It) loop
               declare
                  Rec_Stub : Receiving_Stub renames Value (It).all;
                  pragma Assert (Rec_Stub.Subp_Info /= Null_Address);

                  subtype Subp_Array is RCI_Subp_Info_Array
                    (0 .. Rec_Stub.Subp_Info_Len - 1);

                  package Subp_Info_Addr_Conv is
                     new System.Address_To_Access_Conversions (Subp_Array);

                  Subp_Info : constant Subp_Info_Addr_Conv.Object_Pointer
                    := Subp_Info_Addr_Conv.To_Pointer (Rec_Stub.Subp_Info);
               begin
                  if Rec_Stub.Kind = Pkg_Stub
                    and then To_Lower (Rec_Stub.Name.all) = To_Lower (Pkg_Name)
                  then
                     for J in Subp_Info'Range loop
                        declare
                           Info : RCI_Subp_Info
                             renames Subp_Info (J);

                           subtype Str is
                             String (1 .. Info.Name_Length);

                           package Str_Addr_Conv is
                              new System.Address_To_Access_Conversions
                             (Str);
                        begin
                           if Str_Addr_Conv.To_Pointer (Info.Name).all
                             = Subprogram_Name
                           then
                              Addr     := Info.Addr;
                              Receiver := Rec_Stub.Receiver;
                              exit All_Stubs;
                           end if;
                        end;
                     end loop;
                  end if;
               end;
               Next (It);
            end loop All_Stubs;

            pragma Assert (Addr /= System.Null_Address);

            Build_Local_Reference (Addr, Pkg_Name, Receiver, Subp_Ref);
         end;
      else
         declare
            Ctx_Ref : PSNNC.Ref;
         begin
            PSNNC.Set (Ctx_Ref, Entity_Of (Info.Base_Ref));

            Subp_Ref := PSNNC.Client.Resolve
                          (Ctx_Ref, To_Name (Subprogram_Name, "SUBP"));
         end;
      end if;
   end Get_RAS_Info;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (RACW             : System.Address;
      Type_Name        : String;
      Stub_Tag         : Ada.Tags.Tag;
      Is_RAS           : Boolean;
      Receiver         : access Servant) return PolyORB.References.Ref
   is
      RACW_Stub : RACW_Stub_Type;
      for RACW_Stub'Address use RACW;
      pragma Import (Ada, RACW_Stub);

      CW_RACW_Stub : RACW_Stub_Type'Class
                       renames RACW_Stub_Type'Class (RACW_Stub);

      use type Ada.Tags.Tag;

   begin
      --  Null case

      if RACW = System.Null_Address then
         --  Nothing to do, default initialization for Result is Nil

         return Nil_Ref;

      --  Case of a remote object

      elsif CW_RACW_Stub'Tag = Stub_Tag then
         return Make_Ref (RACW_Stub.Target);

      --  Case of a local object

      elsif Is_RAS then
         --  Remote access to subprogram: use ref from proxy

         declare
            RAS_Proxy : RAS_Proxy_Type;
            for RAS_Proxy'Address use RACW;
            pragma Import (Ada, RAS_Proxy);
         begin
            return Make_Ref (RAS_Proxy.Target);
         end;

      else
         --  Local object

         declare
            Result : PolyORB.References.Ref;
         begin
            Build_Local_Reference (RACW, Type_Name, Receiver, Result);
            return Result;
         end;
      end if;
   end Get_Reference;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access)
   is
      Answer : RACW_Stub_Type_Access;
   begin
      PTM.Enter (Critical_Section);
      Answer := Objects_HTable.Get (Handler);
      if Answer = null then
         Answer := new RACW_Stub_Type;

         --  We leak memory here each time we receive a new unique value of a
         --  remote access to classwide or remote access to subprogram type.

         Answer.Target       := Handler.Target;
         Answer.Asynchronous := Handler.Asynchronous;

         Objects_HTable.Set (Answer, Answer);
      else
         PolyORB.Smart_Pointers.Dec_Usage (Handler.Target);
      end if;
      Handler := Answer;
      PTM.Leave (Critical_Section);
   end Get_Unique_Remote_Pointer;

   ----------
   -- Hash --
   ----------

   function Hash_String is new GNAT.HTable.Hash (Hash_Index);

   function Hash (K : RACW_Stub_Type_Access) return Hash_Index is
      K_Ref : PolyORB.References.Ref;
   begin
      Set (K_Ref, K.Target);
      return Hash_String (PolyORB.References.Image (K_Ref));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      TC_Opaque_Cache := PATC.Build_Sequence_TC (TC_Octet, 0);
      Octet_Sequences_Helper.Initialize
        (Element_TC  => PolyORB.Any.TC_Octet,
         Sequence_TC => TC_Opaque_Cache);

      PTM.Create (Critical_Section);
      PTC.Create (Local_PID_Barrier);
   end Initialize;

   ------------------------
   -- Is_Reference_Valid --
   ------------------------

   function Is_Reference_Valid (R : PolyORB.References.Ref) return Boolean
   is
      use PolyORB.References.Binding;
      use PolyORB.Errors;

      S            : PolyORB.Components.Component_Access;
      Pro          : PolyORB.Binding_Data.Profile_Access;
      Error        : PolyORB.Errors.Error_Container;
   begin

      --  Bind the reference to ensure validity

      Bind (R          => R,
            Local_ORB  => PolyORB.Setup.The_ORB,
            Servant    => S,
            QoS        => (others => null),
            Pro        => Pro,
            Local_Only => False,
            Error      => Error);

      if Found (Error) then
         Catch (Error);
         return False;
      end if;
      return True;
   exception
         when others => return False;
   end Is_Reference_Valid;

   --------------
   -- Make_Ref --
   --------------

   function Make_Ref (The_Entity : PolyORB.Smart_Pointers.Entity_Ptr)
     return PolyORB.References.Ref
   is
      Result : PolyORB.References.Ref;
   begin
      Set_Ref (Result, The_Entity);
      return Result;
   end Make_Ref;

   --------------------
   -- Naming_Context --
   --------------------

   function Naming_Context return PSNNC.Ref is
      R : PolyORB.References.Ref;
   begin
      if PSNNC.Is_Nil (Naming_Context_Cache) then
         PolyORB.References.String_To_Object
           (PolyORB.Parameters.Get_Conf ("dsa", "name_service"),
            R);
         PSNNC.Set (Naming_Context_Cache, Entity_Of (R));
      end if;

      return Naming_Context_Cache;
   end Naming_Context;

   -------------------------------------
   -- Raise_Program_Error_Unknown_Tag --
   -------------------------------------

   procedure Raise_Program_Error_Unknown_Tag
     (E : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity, Ada.Exceptions.Exception_Message (E));
   end Raise_Program_Error_Unknown_Tag;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Buffer_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use PolyORB.Buffers;

      Transfer_Length : constant Stream_Element_Count :=
                          Stream_Element_Count'Min
                            (Remaining (Stream.Buf), Item'Length);
      Data : PolyORB.Opaque.Opaque_Pointer;
   begin
      Extract_Data (Stream.Buf, Data, Transfer_Length);
      Last := Item'First + Transfer_Length - 1;
      declare
         Z_Addr : constant System.Address := Data;
         Z : Stream_Element_Array (Item'First .. Last);
         for Z'Address use Z_Addr;
         pragma Import (Ada, Z);
      begin
         Item (Item'First .. Last) := Z;
      end;
   end Read;

   -----------------
   -- RCI_Locator --
   -----------------

   package body RCI_Locator is

      Info : RCI_Info;

      function Get_RCI_Package_Ref return Object_Ref is
      begin
         if PolyORB.References.Is_Nil (Info.Base_Ref) then
            Info := Retrieve_RCI_Info (RCI_Name);
            Check (RCI_Name, Version, True);
         end if;

         if PolyORB.References.Is_Nil (Info.Base_Ref) then
            raise System.RPC.Communication_Error;

            --  XXX add an informative exception message.
            --  NOTE: Here, we are in calling stubs, so it is OK to raise an
            --  exception that is specific to the DSA applicative personality.

         end if;
         return Info.Base_Ref;
      end Get_RCI_Package_Ref;

   end RCI_Locator;

   ---------------------------------
   -- Register_Obj_Receiving_Stub --
   ---------------------------------

   procedure Register_Obj_Receiving_Stub
     (Name          : String;
      Handler       : Request_Handler_Access;
      Receiver      : Servant_Access)
   is
      use Receiving_Stub_Lists;
   begin
      pragma Assert (Name (Name'Last) = ASCII.NUL);
      Receiver.Handler := Handler;

      Prepend
        (All_Receiving_Stubs,
         Receiving_Stub'
           (Kind                => Obj_Stub,
            Name                =>
              +Name (Name'First .. Name'Last - 1),
            Receiver            => Receiver,
            Version             => null,
            Subp_Info           => Null_Address,
            Subp_Info_Len       => 0,
            Is_All_Calls_Remote => False));

      Receiver.Impl_Info := Private_Info_Access
        (Value (First (All_Receiving_Stubs)));

      declare
         Stub : Receiving_Stub renames Value (First (All_Receiving_Stubs)).all;
      begin
         pragma Debug (C, O ("Setting up RPC receiver: " & Stub.Name.all));
         Setup_Object_RPC_Receiver (Stub.Name.all, Stub.Receiver);
      end;

   end Register_Obj_Receiving_Stub;

   -----------------------------
   -- Retrieve_Receiving_Stub --
   -----------------------------

   function Retrieve_Receiving_Stub (Name : String;
                                     Kind : Receiving_Stub_Kind)
     return Servant_Access
   is
      use Receiving_Stub_Lists;
      It : Receiving_Stub_Lists.Iterator := First (All_Receiving_Stubs);
   begin
      All_Stubs :
      while not Last (It) loop
         if Value (It).all.Kind = Kind
           and then To_Lower (Value (It).all.Name.all) = To_Lower (Name)
         then
            return Value (It).all.Receiver;
         end if;
         Next (It);
      end loop All_Stubs;

      return null;
   end Retrieve_Receiving_Stub;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Name    : Unit_Name;
      Version : String := "") is
   begin
      null;
   end Register_Passive_Package;

   ---------------------------------
   -- Register_Pkg_Receiving_Stub --
   ---------------------------------

   procedure Register_Pkg_Receiving_Stub
     (Name                : String;
      Version             : String;
      Handler             : Request_Handler_Access;
      Receiver            : Servant_Access;
      Subp_Info           : System.Address;
      Subp_Info_Len       : Integer;
      Is_All_Calls_Remote : Boolean)
   is
      use Receiving_Stub_Lists;
   begin
      Receiver.Handler := Handler;
      Prepend
        (All_Receiving_Stubs,
         Receiving_Stub'
           (Kind                => Pkg_Stub,
            Name                => +Name,
            Receiver            => Receiver,
            Version             => +Version,
            Subp_Info           => Subp_Info,
            Subp_Info_Len       => Subp_Info_Len,
            Is_All_Calls_Remote => Is_All_Calls_Remote));

      Receiver.Impl_Info := Private_Info_Access
        (Value (First (All_Receiving_Stubs)));

      declare
         Stub : Receiving_Stub renames Value (First (All_Receiving_Stubs)).all;

         use PolyORB.Errors;
         use PolyORB.ORB;
         use PolyORB.Obj_Adapters;
         use PolyORB.Setup;

         use type PolyORB.POA.Obj_Adapter_Access;

         Error : Error_Container;
         Key : aliased PolyORB.Objects.Object_Id
           := To_Local_Oid (System.Null_Address);

         U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
         Oid : PolyORB.POA_Types.Object_Id_Access;
         Ref : PolyORB.References.Ref;

      begin
         pragma Debug (C, O ("Setting up RPC receiver: " & Stub.Name.all));
         Setup_Object_RPC_Receiver (Stub.Name.all, Stub.Receiver);

         --  Establish a child POA for this stub. For RACWs, this POA will
         --  serve all objects of the same type. For RCIs, this POA will serve
         --  the base object corresponding to the RCI, as well as the
         --  sub-objects corresponding to each subprogram considered as an
         --  object (for RAS).

         PolyORB.POA.Activate_Object
           (Self      => PolyORB.POA.Obj_Adapter_Access
              (Servant_Access (Stub.Receiver).Object_Adapter),
            P_Servant => null,
            Hint      => Key'Unchecked_Access,
            U_Oid     => U_Oid,
            Error     => Error);

         if Found (Error) then
            PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
         end if;

         Oid := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
         Create_Reference
           (The_ORB,
            Oid,
            "DSA:" & Stub.Name.all & ":" & Stub.Version.all,
            Ref);

         PolyORB.Objects.Free (Oid);

         pragma Debug (C, O ("Registering local RCI: " & Stub.Name.all));

         Known_RCIs.Register
           (To_Lower (Stub.Name.all), RCI_Info'
              (Base_Ref            => Ref,
               Is_Local            => True,
               Is_All_Calls_Remote =>
                 Stub.Is_All_Calls_Remote,
               Known_Partition_ID  => False,
               RCI_Partition_ID    => RPC.Partition_ID'First));

         Register_Unit_On_Name_Server
           (Name => To_Lower (Stub.Name.all),
            Kind => "RCI",
            Obj  => Ref);
      end;

   exception

      --  An exception occurring during registration of an RCI is fatal to
      --  the application: terminate PCS and propagate.

      when E : others =>
         O ("Cannot register information for RCI "
             & Name & " with name server.", Error);
         pragma Debug (C, O ("exception raised: "
                             & Ada.Exceptions.Exception_Information (E)));
         PolyORB.Initialization.Shutdown_World (Wait_For_Completion => False);
         raise;
   end Register_Pkg_Receiving_Stub;

   ----------------------------------
   -- Register_Termination_Manager --
   ----------------------------------

   procedure Register_Termination_Manager
     (Ref      : PolyORB.References.Ref;
      Oid      : PolyORB.Objects.Object_Id_Access;
      Address  : System.Address;
      Shutdown : PolyORB.Initialization.Finalizer)
   is
   begin
      The_TM_Ref      := Ref;
      The_TM_Oid      := Oid;
      The_TM_Address  := Address;
      The_TM_Shutdown := Shutdown;
      pragma Debug (C, O ("Registered the termination manager"));
   end Register_Termination_Manager;

   ----------------------------------
   -- Register_Unit_On_Name_Server --
   ----------------------------------

   procedure Register_Unit_On_Name_Server
     (Name : String;
      Kind : String;
      Obj  : PolyORB.References.Ref)
   is
      use Ada.Exceptions;
      Id      : constant PolyORB.Services.Naming.Name := To_Name (Name, Kind);
      Context : PSNNC.Ref;
      Reg_Obj : PolyORB.References.Ref;
   begin
      pragma Debug (C, O ("About to register " & Name & " on nameserver"));

      --  May raise an exception which we do not want to handle in the
      --  following block (failure to establish the naming context is a fatal
      --  error and must be propagated to the caller).

      Context := Naming_Context;

      begin
         Reg_Obj := PSNNC.Client.Resolve (Context, Id);
      exception
         when others =>

            --  Resolution attempt return an authoritative "name not found"
            --  error: register unit now.

            PSNNC.Client.Bind
              (Self => Naming_Context,
               N    => Id,
               Obj  => Obj);
            return;
      end;

      --  Name is present in name server, check validity of the reference it
      --  resolves to.

      if Is_Reference_Valid (Reg_Obj) then
         --  Reference is valid: RCI unit is already declared by another
         --  partition.

         PolyORB.Initialization.Shutdown_World (Wait_For_Completion => False);
         raise Program_Error with Name & " (" & Kind & ") is already declared";

      else
         --  The reference is not valid anymore: we assume the original server
         --  has died, and rebind the name.

         PSNNC.Client.Rebind
           (Self => Naming_Context,
            N    => To_Name (Name, Kind),
            Obj  => Obj);
      end if;
   end Register_Unit_On_Name_Server;

   --------------------
   -- Release_Buffer --
   --------------------

   procedure Release_Buffer (Stream : in out Buffer_Stream_Type) is
   begin
      PolyORB.Buffers.Release (Stream.Buf);
   end Release_Buffer;

   -----------------------
   -- Request_Arguments --
   -----------------------

   procedure Request_Arguments
     (R     :        PolyORB.Requests.Request_Access;
      Args  : in out PolyORB.Any.NVList.Ref)
   is
      Error : PolyORB.Errors.Error_Container;
   begin
      PolyORB.Requests.Arguments (R, Args, Error);
      if PolyORB.Errors.Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Request_Arguments;

   ------------------------------
   -- Request_Raise_Occurrence --
   ------------------------------

   procedure Request_Raise_Occurrence (R : in out Request_Access) is
      use Ada.Exceptions;
      use PolyORB.DSA_P.Exceptions;
      use PolyORB.Exceptions;
   begin
      if not Is_Empty (R.Exception_Info) then
         declare
            E    : constant PolyORB.Any.Any := R.Exception_Info;
            Msg  : constant String :=
                     PolyORB.QoS.Exception_Informations.
                       Get_Exception_Message (R);
         begin
            PolyORB.Requests.Destroy_Request (R);
            Raise_From_Any (E, Msg);
         end;
      end if;
   end Request_Raise_Occurrence;

   ----------------------------------
   -- Register_RACW_In_Name_Server --
   ----------------------------------

   procedure Register_RACW_In_Name_Server
     (Addr     : System.Address;
      Type_Tag : Ada.Tags.Tag;
      Name     : String;
      Kind     : String)
   is
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;
      use PolyORB.Errors;
      use PolyORB.ORB;
      use PolyORB.Obj_Adapters;
      use PolyORB.Setup;

      Key   : aliased PolyORB.Objects.Object_Id := To_Local_Oid (Addr);
      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
      Oid   : PolyORB.POA_Types.Object_Id_Access;
      Error : Error_Container;
      Ref   : PolyORB.References.Ref;
      Receiver : System.Partition_Interface.Servant_Access;

   begin
      pragma Debug (C, O ("Register RACW In Name Server: enter"));
      Receiver := Retrieve_Receiving_Stub
        (Ada.Tags.External_Tag (Type_Tag), Obj_Stub);

      PolyORB.POA.Activate_Object
        (Self      => PolyORB.POA.Obj_Adapter_Access
           (Receiver.Object_Adapter),
         P_Servant => null,
         Hint      => Key'Unchecked_Access,
         U_Oid     => U_Oid,
         Error     => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      Oid := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
      PolyORB.ORB.Create_Reference
        (ORB => The_ORB,
         Oid => Oid,
         Typ => "DSA:" & Name,
         Ref => Ref);

      PolyORB.Objects.Free (Oid);

      Register_Unit_On_Name_Server
        (Name => To_Lower (Name),
         Kind => Kind,
         Obj  => Ref);

      pragma Debug (C, O ("Register RACW In Name Server: leave"));
   end Register_RACW_In_Name_Server;

   ---------------------
   -- Request_Set_Out --
   ---------------------

   procedure Request_Set_Out
     (R     : PolyORB.Requests.Request_Access)
   is
      Error : PolyORB.Errors.Error_Container;
   begin
      PolyORB.Requests.Set_Out_Args (R, Error);
      if PolyORB.Errors.Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Request_Set_Out;

   --------------------
   -- Request_Invoke --
   --------------------

   procedure Request_Invoke
     (R            : PolyORB.Requests.Request_Access;
      Invoke_Flags : PolyORB.Requests.Flags          := 0)
   is
      use PolyORB.QoS;
      use PolyORB.QoS.Term_Manager_Info;
      use PolyORB.Request_QoS;
      use PolyORB.Termination_Activity;
   begin
      Increment_Activity;

      Add_Request_QoS
        (R,
         DSA_TM_Info,
         new QoS_DSA_TM_Info_Parameter'(
           Kind   => DSA_TM_Info,
           TM_Ref => The_TM_Ref)
        );

      PolyORB.Requests.Invoke (R, Invoke_Flags);
   end Request_Invoke;

   -----------------------
   -- Retrieve_RCI_Info --
   -----------------------

   function Retrieve_RCI_Info (Name : String) return RCI_Info is
      use PolyORB.Parameters;
      use PolyORB.Errors;
      use PolyORB.References.Binding;

      LName : constant String := To_Lower (Name);

      Info : RCI_Info;
      Base_Ref : Ref;

      Time_Between_Requests : constant Duration :=
        Get_Conf
          (Section => "dsa",
           Key     => "delay_between_failed_requests",
           Default => 1.0);

      Max_Requests : constant Natural :=
        Get_Conf
          (Section => "dsa",
           Key     => "max_failed_requests",
           Default => 10);

      Retry_Count : Natural := 0;
   begin
      pragma Debug (C, O ("Retrieve RCI info: enter, Name = " & Name));
      Info := Known_RCIs.Lookup (LName, Info);

      --  If RCI information is not available locally, we request it from the
      --  name server. Since some partitions might not be registered yet, we
      --  retry the query up to Max_Requests times.

      if Is_Nil (Info.Base_Ref) then

         --  Unit not known yet: we therefore know that it is remote, and we
         --  need to look it up with the naming service.

         loop
            begin
               Base_Ref := PSNNC.Client.Resolve
                 (Naming_Context, To_Name (LName, "RCI"));

               exit when Is_Reference_Valid (Base_Ref);
               --  Resolve succeeded: exit loop

            exception
               --  Catch all exceptions: we will retry resolution, and bail
               --  out after Max_Requests iterations.

               when others =>
                  null;
            end;

            if Retry_Count = Max_Requests then
               O ("Cannot retrieve information for RCI "
                  & Name & " from name server.", Error);
               raise System.RPC.Communication_Error;
            end if;
            Retry_Count := Retry_Count + 1;
            PolyORB.Tasking.Threads.Relative_Delay (Time_Between_Requests);
         end loop;

         Info := RCI_Info'
           (Base_Ref            => Base_Ref,
            Is_Local            => False,
            Is_All_Calls_Remote => True,
            Known_Partition_ID  => False,
            RCI_Partition_ID    => RPC.Partition_ID'First);

         Known_RCIs.Register (LName, Info);
      end if;
      pragma Debug (C, O ("Retrieve_RCI_Info: leave"));
      return Info;
   end Retrieve_RCI_Info;

   ------------------------------------
   -- Retrieve_RACW_From_Name_Server --
   ------------------------------------

   procedure Retrieve_RACW_From_Name_Server
     (Name     : String;
      Kind     : String;
      Stub_Tag : Ada.Tags.Tag;
      Addr     : out System.Address)
   is
      use PolyORB.Parameters;
      use PolyORB.Errors;
      use PolyORB.References.Binding;

      Reg_Obj     : PolyORB.References.Ref;
      Retry_Count : Natural := 0;

      Time_Between_Requests : constant Duration :=
        Get_Conf
          (Section => "dsa",
           Key     => "delay_between_failed_requests",
           Default => 1.0);

      Max_Requests : constant Natural :=
        Get_Conf
          (Section => "dsa",
           Key     => "max_failed_requests",
           Default => 10);

   begin
      pragma Debug (C, O ("Retrieve RACW From Name Server: enter"));

      loop
         begin
            Reg_Obj := PSNNC.Client.Resolve
              (Naming_Context, To_Name (Name, Kind));

            exit when Is_Reference_Valid (Reg_Obj);
            --  Resolve succeeded: exit loop

         exception
               --  Catch all exceptions: we will retry resolution, and bail
               --  out after Max_Requests iterations.

            when others =>
               null;
         end;

         if Retry_Count = Max_Requests then
            O ("Cannot retrieve information for"
               & "RACW " & Name & ":" & Kind & " from name server.", Error);
            raise System.RPC.Communication_Error;
         end if;
         Retry_Count := Retry_Count + 1;
         PolyORB.Tasking.Threads.Relative_Delay (Time_Between_Requests);
      end loop;

      Addr := Get_RACW
        (Ref          => Reg_Obj,
         Stub_Tag     => Stub_Tag,
         Is_RAS       => False,
         Asynchronous => True);

      pragma Debug (C, O ("Retrieve RACW From Name Server: leave"));

   end Retrieve_RACW_From_Name_Server;

   --------------------
   -- Same_Partition --
   --------------------

   function Same_Partition
     (Left  : access RACW_Stub_Type;
      Right : access RACW_Stub_Type) return Boolean
   is
   begin
      return Same_Node (Make_Ref (Left.Target), Make_Ref (Right.Target));
   end Same_Partition;

   -------------------------------
   -- Setup_Object_RPC_Receiver --
   -------------------------------

   procedure Setup_Object_RPC_Receiver
     (Name            : String;
      Default_Servant : Servant_Access)
   is
      use PolyORB.Errors;
      use PolyORB.POA;
      use PolyORB.POA_Config;
      use PolyORB.POA_Config.RACWs;
      use PolyORB.POA_Manager;
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;

      POA : Obj_Adapter_Access;
      PName : constant PolyORB.Types.String
        := PolyORB.Types.String (To_PolyORB_String (Name));

      Error : Error_Container;
   begin
      --  NOTE: Actually this does more than set up an RPC receiver. A TypeCode
      --  corresponding to the RACW is also constructed (and this is vital also
      --  on the client side.)

      Default_Servant.Obj_TypeCode := PolyORB.Any.TypeCode.TC_Object;
      PATC.Add_Parameter
        (Default_Servant.Obj_TypeCode, To_Any (PName));
      PATC.Add_Parameter
        (Default_Servant.Obj_TypeCode, TA_Std_String ("DSA:" & Name & ":1.0"));

      if RACW_POA_Config = null then
         return;
      end if;

      Create_POA
        (Self         => PolyORB.POA.Obj_Adapter_Access
         (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB)),
         Adapter_Name => Name,
         A_POAManager => null,
         Policies     => Default_Policies (RACW_POA_Config.all),
         POA          => POA,
         Error        => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      POA.Default_Servant := PolyORB.Servants.Servant_Access
        (Default_Servant);

      Default_Servant.Object_Adapter :=
        PolyORB.Obj_Adapters.Obj_Adapter_Access (POA);
      pragma Assert (Default_Servant.Object_Adapter /= null);

      Activate (POAManager_Access (Entity_Of (POA.POA_Manager)), Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

   end Setup_Object_RPC_Receiver;

   --------------
   -- TC_Build --
   --------------

   --  ??? This function should be replaced by a call to Build_Complex_TC

   function TC_Build
     (Base       : PolyORB.Any.TypeCode.Local_Ref;
      Parameters : Any_Array) return PolyORB.Any.TypeCode.Local_Ref
   is
   begin
      for J in Parameters'Range loop
         PATC.Add_Parameter (Base, Parameters (J));
      end loop;
      return Base;
   end TC_Build;

   ---------------
   -- TC_Opaque --
   ---------------

   function TC_Opaque return PATC.Local_Ref is
   begin
      return TC_Opaque_Cache;
   end TC_Opaque;

   ------------
   -- To_Any --
   ------------

   function TA_A (Item : DSAT.Any_Container_Ptr) return PolyORB.Any.Any is
      pragma Warnings (Off);
      --  No aliasing issues since DSAT.Any_Container_Ptr always originally
      --  comes from a PolyORB.Any.Any_Container_Ptr.

      function To_PolyORB_ACP is new Ada.Unchecked_Conversion
        (DSAT.Any_Container_Ptr, PolyORB.Any.Any_Container_Ptr);
      pragma Warnings (On);
      Item_A : PolyORB.Any.Any;
   begin
      PolyORB.Any.Set_Container (Item_A, To_PolyORB_ACP (Item));
      return PolyORB.Any.To_Any (Item_A);
   end TA_A;

   function TA_B (Item : Boolean) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Boolean (Item));
   end TA_B;

   function TA_C (Item : Character) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Char (Item));
   end TA_C;

   function TA_F (Item : Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Float (Item));
   end TA_F;

   function TA_I (Item : Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long (Item));
   end TA_I;

   function TA_U (Item : Unsigned) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Long (Item));
   end TA_U;

   function TA_LF (Item : Long_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Double (Item));
   end TA_LF;

   function TA_LI (Item : Long_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long (Item));
   end TA_LI;

   function TA_LU (Item : Long_Unsigned) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Long (Item));
   end TA_LU;

   function TA_LLF (Item : Long_Long_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long_Double (Item));
   end TA_LLF;

   function TA_LLI (Item : Long_Long_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long_Long (Item));
   end TA_LLI;

   function TA_LLU (Item : Long_Long_Unsigned) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Long_Long (Item));
   end TA_LLU;

   function TA_SF (Item : Short_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Float (Item));
   end TA_SF;

   function TA_SI (Item : Short_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Short (Item));
   end TA_SI;

   function TA_SU (Item : Short_Unsigned) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Short (Item));
   end TA_SU;

   function TA_SSI (Item : Short_Short_Integer) return PolyORB.Any.Any is
      function To_Octet is new Ada.Unchecked_Conversion
        (Short_Short_Integer, PolyORB.Types.Octet);
   begin
      return To_Any (To_Octet (Item));
   end TA_SSI;

   function TA_SSU (Item : Short_Short_Unsigned) return PolyORB.Any.Any is
      function To_Octet is new Ada.Unchecked_Conversion
        (Short_Short_Unsigned, PolyORB.Types.Octet);
   begin
      return To_Any (To_Octet (Item));
   end TA_SSU;

   function TA_WC (Item : Wide_Character) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Wchar (Item));
   end TA_WC;

   function TA_String
     (S : Ada.Strings.Unbounded.Unbounded_String) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.String (S));
   end TA_String;

   function TA_Std_String (S : String) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.To_PolyORB_String (S));
   end TA_Std_String;

   -------------
   -- To_Name --
   -------------

   function To_Name (Id, Kind : String) return PolyORB.Services.Naming.Name is
      use PolyORB.Services.Naming.SEQUENCE_NameComponent;
   begin
      return PolyORB.Services.Naming.Name
        (To_Sequence
         ((1 => (id   => PolyORB.Services.Naming.To_PolyORB_String (Id),
                 kind => PolyORB.Services.Naming.To_PolyORB_String (Kind)))));
   end To_Name;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Buffer_Stream_Type;
      Item   : Stream_Element_Array)
   is
      use PolyORB.Buffers;

      Data : PolyORB.Opaque.Opaque_Pointer;
   begin
      Allocate_And_Insert_Cooked_Data (Stream.Buf, Item'Length, Data);
      declare
         Z_Addr : constant System.Address := Data;
         Z : Stream_Element_Array (Item'Range);
         for Z'Address use Z_Addr;
         pragma Import (Ada, Z);
      begin
         Z := Item;
      end;
   end Write;

   ------------
   -- Detach --
   ------------

   procedure Detach is
      procedure C_Detach;
      pragma Import (C, C_Detach, "__PolyORB_detach");
   begin
      C_Detach;
   end Detach;

   ---------------------
   -- Shutdown_Module --
   ---------------------

   procedure Shutdown_Module (Wait_For_Completion : Boolean);

   procedure Shutdown_Module (Wait_For_Completion : Boolean) is
      use type PolyORB.Initialization.Finalizer;
   begin
      --  Shut down the local termination manager, if it has already been
      --  registered.

      if The_TM_Shutdown /= null then
         The_TM_Shutdown (Wait_For_Completion);
      end if;
   end Shutdown_Module;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"dsa",
       Conflicts => Empty,
       Depends   => +"orb"
       & "parameters"
       & "poa"
       & "poa_config.racws"
       & "object_adapter"
       & "naming.Helper"
       & "naming.NamingContext.Helper"
       & "tasking.condition_variables"
       & "tasking.mutexes"
       & "access_points?"
       & "binding_factories"
       & "references"
       & "request_qos.dsa_tm_info"
       & "termination.activity",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => Shutdown_Module'Access));

   --  We initialize PolyORB, so that once s-parint is elaborated, the PCS is
   --  up and running, ready to process RPCs.

   Initialize_World;

   --  Run additional tasks if needed

   PolyORB.Partition_Elaboration.Run_Additional_Tasks;

   --  Elaboration of the PCS is finished, launch others partitions if needed

   PolyORB.Partition_Elaboration.Full_Launch;

   --  Detach partition if needed

   if PolyORB.Parameters.Get_Conf
        (Section => "dsa",
         Key     => "detach",
         Default => False)
   then
      Detach;
   end if;
end System.Partition_Interface;
