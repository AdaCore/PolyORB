with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System.Address_To_Access_Conversions;
with System.RPC;

with GNAT.HTable;

with CORBA.Object;
with PolyORB.CORBA_P.Naming_Tools;
with CosNaming.Helper;
--  RCI package references are managed through the CORBA
--  naming service. RCIs also act as naming contexts themselves
--  for the purpose of providing access to each of their subprograms
--  as objects.

with PolyORB.Binding_Data;
with PolyORB.Dynamic_Dict;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Obj_Adapters;
with PolyORB.Objects.Interface;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.References;
with PolyORB.Soft_Links;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;

--  XXX the following are dependant on configuration options
--  and should be moved to a generated unit (à la s-garela).
--  (an OA is required only on server units; any OA is OK
--  for RCIs without RACWs, but RACWs require a POA).

with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config.Minimum;
with PolyORB.POA_Manager;

package body System.PolyORB_Interface is

   use Ada.Characters.Handling;
   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.References;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("system.polyorb_interface");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------------------
   -- Local declarations --
   ------------------------

   --  During elaboration, each RCI package and each distributed
   --  object type registers a Receiving_Stub entry.

   type Receiving_Stub_Kind is (Obj_Stub, Pkg_Stub);
   type RCI_Subp_Info_Array_Access is
     access all RCI_Subp_Info_Array;

   type Receiving_Stub is new Private_Info with record
      Kind                : Receiving_Stub_Kind;
      --  Indicates whetger this info is relative to a
      --  RACW type or a RCI.

      Name                : String_Ptr;
      --  Fully qualified name of the RACW or RCI

      Version             : String_Ptr;
      --  For RCIs only: library unit version

      Receiver            : Servant_Access;
      --  The RPC receiver (servant) object

      Is_All_Calls_Remote : Boolean;
      --  For RCIs only: true iff a pragma All_Calls_Remote
      --  applies to this unit.

      Subp_Info           : RCI_Subp_Info_Array_Access;
      --  For RCIs only: mapping of RCI subprogram names to
      --  addresses.

   end record;

   package Receiving_Stub_Lists is new PolyORB.Utils.Chained_Lists
     (Receiving_Stub);

   All_Receiving_Stubs : Receiving_Stub_Lists.List;

   procedure Initialize;
   --  Initialization procedure to be called during the
   --  global PolyORB initialization.

   --  A map of all known RCIs is maintained.

   type RCI_Info is record

      Is_All_Calls_Remote : Boolean := True;
      --  True if the package is remote, or if a
      --  pragma All_Call_Remotes applies.

      Base_Ref            : Object_Ref;
      --  The main reference for this package.

      Is_Local            : Boolean := False;
      --  True if the package is assigned on this partition.

   end record;

   No_RCI_Info : RCI_Info;

   package Known_RCIs is new PolyORB.Dynamic_Dict (RCI_Info, No_RCI_Info);

   function Retrieve_RCI_Info (Name : String) return RCI_Info;
   --  Retrieve RCI information for a local or remote RCI package.

   --  To limit the amount of memory leaked by the use of
   --  distributed object stub types, these are referenced
   --  in a hash table and reused whenever possible.

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

   procedure Free is
      new Ada.Unchecked_Deallocation (RACW_Stub_Type, RACW_Stub_Type_Access);

   --  When a RACW must be constructed to designate a local object,
   --  an object identifier is created using the address of the object.

   use type Ada.Streams.Stream_Element_Offset;
   subtype Local_Oid is PolyORB.Objects.Object_Id
     (1 .. System.Address'Size / 8);
   function To_Local_Oid is
      new Ada.Unchecked_Conversion (System.Address, Local_Oid);
   function To_Address is
      new Ada.Unchecked_Conversion (Local_Oid, System.Address);

   procedure Setup_Object_RPC_Receiver
     (Name            : String;
      Default_Servant : Servant_Access);
   --  Setup an object adapter to receive method invocation
   --  requests for distributed object type Name.
   --  Use the specified POA configuration (which must include
   --  the USER_ID, NON_RETAIN and USE_DEFAULT_SERVANT policies).
   --  The components of Servant are set appropriately.

   -------------------------
   -- Any_Aggregate_Build --
   -------------------------

   function Any_Aggregate_Build
     (TypeCode : PolyORB.Any.TypeCode.Object;
      Contents : Any_Array)
      return Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TypeCode);
   begin
      for J in Contents'Range loop
         Add_Aggregate_Element (Result, Contents (J));
      end loop;
      return Result;
   end Any_Aggregate_Build;

   ------------------------
   -- Caseless_String_Eq --
   ------------------------

   function Caseless_String_Eq (S1, S2 : String) return Boolean
   is
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

   ---------------------
   -- Compare_Content --
   ---------------------

   function Compare_Content (Left, Right : RACW_Stub_Type_Access)
     return Boolean
   is
      use Interfaces;
      use System.RPC;

      Left_Object, Right_Object : PolyORB.References.Ref;

   begin
      Set (Left_Object, Left.Target);
      Set (Right_Object, Right.Target);

      return Left /= null and then Right /= null
        and then Left.Origin = Right.Origin
        and then Left.Receiver = Right.Receiver
        and then Left.Addr = Right.Addr
        and then PolyORB.References.Is_Same_Object
        (Left_Object, Right_Object);
   end Compare_Content;

   --------------
   -- From_Any --
   --------------

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

   function FA_LF (Item : PolyORB.Any.Any) return Long_Float is
   begin
      return Long_Float (PolyORB.Types.Double'(From_Any (Item)));
   end FA_LF;

   function FA_LI (Item : PolyORB.Any.Any) return Long_Integer is
   begin
      return Long_Integer (PolyORB.Types.Long'(From_Any (Item)));
   end FA_LI;

   function FA_LLF (Item : PolyORB.Any.Any) return Long_Long_Float is
   begin
      return Long_Long_Float (PolyORB.Types.Long_Double'(From_Any (Item)));
   end FA_LLF;

   function FA_LLI (Item : PolyORB.Any.Any) return Long_Long_Integer is
   begin
      return Long_Long_Integer (PolyORB.Types.Long_Long'(From_Any (Item)));
   end FA_LLI;

   function FA_SF (Item : PolyORB.Any.Any) return Short_Float is
   begin
      return Short_Float (PolyORB.Types.Float'(From_Any (Item)));
   end FA_SF;

   function FA_SI (Item : PolyORB.Any.Any) return Short_Integer is
   begin
      return Short_Integer (PolyORB.Types.Short'(From_Any (Item)));
   end FA_SI;

   function FA_SSI (Item : PolyORB.Any.Any) return Short_Short_Integer is
   begin
      return Short_Short_Integer (PolyORB.Types.Octet'(From_Any (Item)));
   end FA_SSI;

   function FA_WC (Item : PolyORB.Any.Any) return Wide_Character is
   begin
      return Wide_Character (PolyORB.Types.Wchar'(From_Any (Item)));
   end FA_WC;

   function FA_String (Item : PolyORB.Any.Any) return String is
   begin
      return PolyORB.Types.To_String (From_Any (Item));
   end FA_String;

   ----------
   -- Hash --
   ----------

   function Hash_String is new GNAT.HTable.Hash (Hash_Index);

   function Hash (K : RACW_Stub_Type_Access) return Hash_Index
   is
      K_Ref : PolyORB.References.Ref;
   begin
      Set (K_Ref, K.Target);
      return Hash_String (PolyORB.References.Image (K_Ref));
   end Hash;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Self : access Servant;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class
   is
      use PolyORB.Objects.Interface;

      Result : PolyORB.Components.Null_Message;
   begin
      if Msg in Execute_Request then
         declare
            EMsg : Execute_Request renames Execute_Request (Msg);
         begin
            if Receiving_Stub (Self.Impl_Info.all).Kind = Pkg_Stub
              and then
              PolyORB.Types.To_Standard_String (EMsg.Req.Operation)
              = "resolve"
            then

               --  Code extracted from CosNaming::NamingContext
               --  IDL skel.

               declare
                  use CosNaming;
                  --  For exception names

                  n             : CosNaming.Name;
                  Arg_Name_n  : constant PolyORB.Types.Identifier :=
                    To_PolyORB_String ("n");
                  Argument_n  : Any :=
                    CosNaming.Helper.To_Any (n);

                  Result      : Object_Ref;
                  Argument_Result : Any;
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

                  begin
                     --  Convert arguments from their Any

                     n := CosNaming.Helper.From_Any (Argument_n);

                     --  Call implementation
                     Get_RAS_Ref
                       (Receiving_Stub (Self.Impl_Info.all).Name.all,
                        n.Name, Result);

--                 exception
--                    when E : NotFound =>
--                       declare
--                          Members : NotFound_Members;
--                       begin
--                          Get_Members (E, Members);
--                          CORBA.ServerRequest.Set_Exception
--                            (EMsg.Req,
--                             CosNaming.NamingContext.Helper.To_Any (Members));
--                          return;
--                       end;
--                    when E : CannotProceed =>
--                       declare
--                          Members : CannotProceed_Members;
--                       begin
--                          Get_Members (E, Members);
--                          CORBA.ServerRequest.Set_Exception
--                            (EMsg.Req,
--                             CosNaming.NamingContext.Helper.To_Any (Members));
--                          return;
--                       end;
--                    when E : InvalidName =>
--                       declare
--                          Members : InvalidName_Members;
--                       begin
--                          Get_Members (E, Members);
--                          CORBA.ServerRequest.Set_Exception
--                            (Request,
--                             CosNaming.NamingContext.Helper.To_Any (Members));
--                          return;
--                       end;
                  end;

                  -- Set Result

                  CORBA.ServerRequest.Set_Result
                    (Request,
                     CORBA.Object.Helper.To_Any (Result));
                  return;
               end;

            else
               pragma Assert (Self.Handler /= null);
               Self.Handler.all (EMsg.Req);
            end if;

            return Executed_Request'(Req => EMsg.Req);
         end;
      end if;
      return Result;
   end Handle_Message;

   ----------------
   -- Initialize --
   ----------------

   Root_POA_Object : PolyORB.POA.Obj_Adapter_Access;

   procedure Initialize
   is
      use PolyORB;
      use type PolyORB.POA.Obj_Adapter_Access;

      use Receiving_Stub_Lists;

      It : Iterator;
   begin
      pragma Assert (Root_POA_Object = null);
      pragma Debug (O ("Initializing default POA configuration..."));
      POA_Config.Set_Configuration
        (new POA_Config.Minimum.Minimum_Configuration);

      pragma Debug (O ("Initializing root POA..."));
      Root_POA_Object := new POA.Basic_POA.Basic_Obj_Adapter;
      POA.Basic_POA.Create
        (POA.Basic_POA.Basic_Obj_Adapter (Root_POA_Object.all)'Access);

      ORB.Set_Object_Adapter
        (Setup.The_ORB, Obj_Adapters.Obj_Adapter_Access (Root_POA_Object));
      --  Link object adapter with ORB.

      POA_Manager.Activate
        (POA_Manager.POAManager_Access
           (POA_Manager.Entity_Of (Root_POA_Object.POA_Manager)));

      pragma Debug (O ("Initializing DSA library units"));
      It := First (All_Receiving_Stubs);
      while not Last (It) loop
         declare
            use PolyORB.Obj_Adapters;
            use PolyORB.ORB;
            use PolyORB.Setup;

            Stub : Receiving_Stub renames Value (It).all;

         begin
            Setup_Object_RPC_Receiver
              (Stub.Name.all, Stub.Receiver);
            --  Establish a child POA for this stub. For RACWs,
            --  this POA will serve all objects of the same type.
            --  For RCIs, this POA will serve the base object
            --  corresponding to the RCI, as well as the sub-objects
            --  corresponding to each subprogram considered as
            --  an object (for RAS).

            case Stub.Kind is
               when Obj_Stub =>
                  null;

               when Pkg_Stub =>
                  declare
                     Key : aliased PolyORB.Objects.Object_Id
                       := To_Local_Oid (System.Null_Address);

                     Oid : aliased PolyORB.Objects.Object_Id
                       := PolyORB.Obj_Adapters.Export
                       (OA  => Servant_Access (Stub.Receiver).Object_Adapter,
                        Obj => null,
                        Key => Key'Unchecked_Access);

                     Ref : PolyORB.References.Ref;
                  begin
                     Create_Reference
                       (The_ORB, Oid'Access,
                        "DSA:" & Stub.Name.all & ":" & Stub.Version.all,
                        Ref);
                     Known_RCIs.Register
                       (Stub.Name.all, RCI_Info'
                          (Base_Ref            => Ref,
                           Is_Local            => True,
                           Is_All_Calls_Remote =>
                             Stub.Is_All_Calls_Remote));

                     declare
                        CRef : CORBA.Object.Ref;
                        --  XXX SHOULD NOT DEPEND ON CORBA!!!!!
                     begin
                        CORBA.Object.Convert_To_CORBA_Ref (Ref, CRef);
                        PolyORB.CORBA_P.Naming_Tools.Register
                          (Name => To_Lower (Stub.Name.all) & ".RCI",
                           Ref => CRef, Rebind => True);
                        --  XXX Using the CORBA naming service is not
                        --  necessarily a good idea. Alternative design:
                        --  use a Boot_Server distributed object dedicated
                        --  to DSA applications (may be required for
                        --  validation, because we need to somehow assign
                        --  Partition_Ids).
                     end;
                  end;
            end case;
         end;
         Next (It);
      end loop;
      pragma Debug (O ("Done initializing DSA."));

      --  Note: currently we keep the All_Receiving_Stubs list
      --  in memory, because Get_RAS_Ref uses it at a later point.
      --  An alternative (better) design would store the relevant
      --  RCI subprogram info in hash tables, and get rid of
      --  the list after initialization.

   end Initialize;

   -----------------------
   -- Get_Local_Address --
   -----------------------

   procedure Get_Local_Address
     (Ref      : PolyORB.References.Ref;
      Is_Local : out Boolean;
      Addr     : out System.Address)
   is
      Profiles : constant Profile_Array
        := PolyORB.References.Profiles_Of (Ref);
   begin
      for J in Profiles'Range loop
         if PolyORB.ORB.Is_Profile_Local
           (PolyORB.Setup.The_ORB,
            Profiles (J))
         then
            begin
               declare
                  Key : constant PolyORB.Objects.Object_Id
                    := PolyORB.Obj_Adapters.Object_Key
                    (OA => PolyORB.Obj_Adapters.Obj_Adapter_Access
                       (Root_POA_Object),
                     Id => PolyORB.Binding_Data.Get_Object_Key
                       (Profiles (J).all));
               begin
                  Is_Local := True;
                  Addr := To_Address (Key (Key'Range));
                  return;
               end;
            exception
               when PolyORB.POA.Invalid_Object_Id =>
                  --  This object identifier does not have a
                  --  user-assigned object key.
                  null;

               when others =>
                  raise;
            end;
         end if;
      end loop;

      Is_Local := False;
      Addr := Null_Address;
   end Get_Local_Address;

   -----------------
   -- Get_RAS_Ref --
   -----------------

   procedure Get_RAS_Ref
     (Pkg_Name        :     String;
      Subprogram_Name :     String;
      Subp_Ref        : out Object_Ref)
   is
      Info : constant RCI_Info := Retrieve_RCI_Info (Pkg_Name);

   begin
      if Info.Is_Local then
         --  Retrieve subprogram address using subprogram name
         --  and subprogram table. Warning: the name used
         --  MUST be the distribution-name (with overload
         --  suffix, where appropriate.)

         declare
            use Receiving_Stub_Lists;
            It : Receiving_Stub_Lists.Iterator :=
              First (All_Receiving_Stubs);

            package Str_Addr_Conversion is
               new System.Address_To_Access_Conversions (String);
            use Str_Addr_Conversion;

            Addr : System.Address := System.Null_Address;
            Receiver : Servant_Access := null;

         begin

            --  XXX
            --  The following is ugly and inefficient (two levels
            --  of linear search) and should probably be optimized
            --  in some way.

            All_Stubs :
            while not Last (It) loop
               declare
                  S : Receiving_Stub renames Value (It).all;
               begin
                  if S.Kind = Pkg_Stub and then S.Name.all = Pkg_Name then
                     for J in S.Subp_Info'Range loop
                        if To_Pointer (S.Subp_Info (J).Name).all
                          = Subprogram_Name
                        then
                           Addr := S.Subp_Info (J).Addr;
                           Receiver := S.Receiver;
                           exit All_Stubs;
                        end if;
                     end loop;
                  end if;
               end;
            end loop All_Stubs;

            pragma Assert (Addr /= System.Null_Address);

            Get_Reference
              (Addr, Pkg_Name, Receiver, Subp_Ref);
         end;
      else
         declare
            Ctx : CosNaming.NamingContext.Ref;
         begin
            CORBA.Object.Convert_To_CORBA_Ref (Info.Base_Ref, Ctx);

            Subp_Ref := CORBA.Object.To_PolyORB_Ref
              (PolyORB.CORBA_P.Naming_Tools.Locate
                 (Ctx, Subprogram_Name & ".SUBP"));
         end;
      end if;
   end Get_RAS_Ref;

   -------------------
   -- Get_Reference --
   -------------------

   procedure Get_Reference
     (Addr     :        System.Address;
      Typ      :        String;
      Receiver : access Servant;
      Ref      :    out PolyORB.References.Ref)
   is
      Last : Integer := Typ'Last;
   begin
      if Last in Typ'Range and then Typ (Last) = ASCII.NUL then
         Last := Last - 1;
      end if;

      if Addr /= Null_Address then
         declare
            Key : aliased PolyORB.Objects.Object_Id
              := To_Local_Oid (Addr);

            Oid : aliased PolyORB.Objects.Object_Id
              := PolyORB.Obj_Adapters.Export
              (OA  => Servant_Access (Receiver).Object_Adapter,
               Obj => null,
               Key => Key'Unchecked_Access);

         begin
            PolyORB.ORB.Create_Reference
              (PolyORB.Setup.The_ORB, Oid'Access,
               "DSA:" & Typ (Typ'First .. Last), Ref);
         end;
      end if;
   exception
      when E : others =>
         pragma Debug
           (O ("Get_Reference: got exception "
                 & Ada.Exceptions.Exception_Information (E)));
         pragma Debug (O ("returning a nil ref."));
         null;

   end Get_Reference;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access)
   is
      Answer : RACW_Stub_Type_Access;
   begin
      PolyORB.Soft_Links.Enter_Critical_Section;
      Answer := Objects_HTable.Get (Handler);
      if Answer = null then
         Objects_HTable.Set (Handler, Handler);
      else
         PolyORB.Smart_Pointers.Dec_Usage (Handler.Target);
         --  Corresponds to a call to Adjust in generated code,
         --  at the time Handler was instantiated.
         Free (Handler);
         Handler := Answer;
      end if;
      PolyORB.Soft_Links.Leave_Critical_Section;
   end Get_Unique_Remote_Pointer;

   -----------------
   -- RCI_Locator --
   -----------------

   package body RCI_Locator is

      Info : RCI_Info;

      function Get_RCI_Package_Ref
        return Object_Ref is
      begin
         if PolyORB.References.Is_Nil (Info.Base_Ref) then
            Info := Retrieve_RCI_Info (RCI_Name);
         end if;

         if PolyORB.References.Is_Nil (Info.Base_Ref) then
            raise System.RPC.Communication_Error;
            --  XXX add an informative exception message.
            --  NOTE: Here, we are in calling stubs, so it is
            --  OK to raise an exception that is specific to
            --  the DSA applicative personality.
         end if;
         return Info.Base_Ref;
      end Get_RCI_Package_Ref;

   end RCI_Locator;

   ---------------------------------
   -- Register_Obj_Receiving_Stub --
   ---------------------------------

   procedure Register_Obj_Receiving_Stub
     (Name          : in String;
      Handler       : in Request_Handler_Access;
      Receiver      : in Servant_Access)
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
            Subp_Info           => null,
            Is_All_Calls_Remote => False));
      Receiver.Impl_Info := Private_Info_Access
        (Value (First (All_Receiving_Stubs)));
   end Register_Obj_Receiving_Stub;

   ---------------------------------
   -- Register_Pkg_Receiving_Stub --
   ---------------------------------

   procedure Register_Pkg_Receiving_Stub
     (Name                : String;
      Version             : String;
      Handler             : Request_Handler_Access;
      Receiver            : Servant_Access;
      Subp_Info           : access RCI_Subp_Info_Array;
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
            Subp_Info           => RCI_Subp_Info_Array_Access
              (Subp_Info),
            Is_All_Calls_Remote => Is_All_Calls_Remote));
      Receiver.Impl_Info := Private_Info_Access
        (Value (First (All_Receiving_Stubs)));
   end Register_Pkg_Receiving_Stub;

   -----------------------
   -- Retrieve_RCI_Info --
   -----------------------

   function Retrieve_RCI_Info (Name : String) return RCI_Info
   is
      Info : RCI_Info;
   begin
      Info := Known_RCIs.Lookup (Name, Info);
      if PolyORB.References.Is_Nil (Info.Base_Ref) then
         --  Not known yet: we therefore know that it is remote,
         --  and that we need to look it up with the naming service.
         Info := RCI_Info'
           (Base_Ref => CORBA.Object.To_PolyORB_Ref
              (PolyORB.CORBA_P.Naming_Tools.Locate
                 (To_Lower (Name) & ".RCI")),
            Is_Local => False,
            Is_All_Calls_Remote => True);
         Known_RCIs.Register (Name, Info);
      end if;
      return Info;
   end Retrieve_RCI_Info;

   -------------------------------
   -- Setup_Object_RPC_Receiver --
   -------------------------------

   procedure Setup_Object_RPC_Receiver
     (Name            : String;
      Default_Servant : Servant_Access)
   is
      use PolyORB.POA;
      use PolyORB.POA_Config;
      use PolyORB.POA_Manager;

      POA : Obj_Adapter_Access;
      PName : constant PolyORB.Types.String
        := PolyORB.Types.String (To_PolyORB_String (Name));
   begin
      --  NOTE: Actually this does more than set up an RPC
      --  receiver. A TypeCode corresponding to the RACW is
      --  also constructed (and this is vital also on the
      --  client side.)

      Default_Servant.Obj_TypeCode := PolyORB.Any.TC_Object;
      PolyORB.Any.TypeCode.Add_Parameter
        (Default_Servant.Obj_TypeCode,
         To_Any (PName));
      PolyORB.Any.TypeCode.Add_Parameter
        (Default_Servant.Obj_TypeCode,
         TA_String ("DSA:" & Name & ":1.0"));

      if RACW_POA_Config = null then
         return;
      end if;

      POA := Create_POA
        (Self         => Root_POA_Object,
         Adapter_Name => PName,
         A_POAManager => null,
         Policies     => Default_Policies (RACW_POA_Config.all));

      POA.Default_Servant := PolyORB.Objects.Servant_Access
        (Default_Servant);

      Default_Servant.Object_Adapter :=
        PolyORB.Obj_Adapters.Obj_Adapter_Access (POA);

      Activate (POAManager_Access (Entity_Of (POA.POA_Manager)));
   end Setup_Object_RPC_Receiver;

   ------------
   -- To_Any --
   ------------

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

   function TA_LF (Item : Long_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Double (Item));
   end TA_LF;

   function TA_LI (Item : Long_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long (Item));
   end TA_LI;

   function TA_LLF (Item : Long_Long_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long_Double (Item));
   end TA_LLF;

   function TA_LLI (Item : Long_Long_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long_Long (Item));
   end TA_LLI;

   function TA_SF (Item : Short_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Float (Item));
   end TA_SF;

   function TA_SI (Item : Short_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Short (Item));
   end TA_SI;

   function TA_SSI (Item : Short_Short_Integer) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Octet (Item));
   end TA_SSI;

   function TA_WC (Item : Wide_Character) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Wchar (Item));
   end TA_WC;

   function TA_String (S : String) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.To_PolyORB_String (S));
   end TA_String;

   --------------
   -- TC_Build --
   --------------

   function TC_Build
     (Base : PolyORB.Any.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.Any.TypeCode.Object
   is
      Result : PolyORB.Any.TypeCode.Object
        := Base;
   begin
      for I in Parameters'Range loop
         PolyORB.Any.TypeCode.Add_Parameter
           (Result, Parameters (I));
      end loop;
      return Result;
   end TC_Build;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name => +"dsa",
       Conflicts => Empty,
       Depends => +"orb"
         & "initial_references"
         & "access_points?"
         & "poa_config.racws?",
       Provides => Empty,
       Init => Initialize'Access));
end System.PolyORB_Interface;
