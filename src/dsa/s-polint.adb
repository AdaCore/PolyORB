------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             S Y S T E M . P O L Y O R B _ I N T E R F A C E              --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with System.Address_To_Access_Conversions;
with System.RPC;

with GNAT.HTable;

with CORBA.Object;
with PolyORB.CORBA_P.Naming_Tools;
with CosNaming.Helper;
with CosNaming.NamingContext;
--  XXX THESE DEPS UPON CORBA MUST BE REMOVED!!!!!
--  RCI package references are managed through the CORBA
--  naming service. RCIs also act as naming contexts themselves
--  for the purpose of providing access to each of their subprograms
--  as objects.

with PolyORB.Binding_Data;
with PolyORB.DSA_P.Partitions;
with PolyORB.Dynamic_Dict;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Obj_Adapters;
with PolyORB.Objects.Interface;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.References;
with PolyORB.Servants;
with PolyORB.Tasking.Soft_Links;
with PolyORB.Utils.Strings.Lists;

--  XXX the following are dependant on configuration options
--  and should be moved to a generated unit (à la s-garela).
--  (an OA is required only on server units; any OA is OK
--  for RCIs without RACWs, but RACWs require a POA).

with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config.Minimum;
with PolyORB.POA_Manager;
with PolyORB.Setup.Proxies_POA;

package body System.PolyORB_Interface is

   use Ada.Characters.Handling;
   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.References;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("system.polyorb_interface");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------------------------------------------------
   -- Special operation names for remote call interface objets --
   --------------------------------------------------------------

   Op_Resolve : constant String := "resolve";
   --  Corresponds to the CORBA CosNaming::NamingContext::resolve operation.

   Op_Get_Partition_Id : constant String := "_get_partition_id";
   --  Get the DSA partition identifier for the partition that hosts an RCI?

   ------------------------
   -- Local declarations --
   ------------------------

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
   pragma Warnings (Off, No_RCI_Info);
   --  Never assigned a value.

   package Known_RCIs is new PolyORB.Dynamic_Dict (RCI_Info, No_RCI_Info);
   --  This list is keyed with the lowercased full names of the
   --  RCI units.

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

   function DSA_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence)
      return Any;
   --  Construct an Any from an Ada exception raised by a servant.

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

   function DSA_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence)
      return Any
   is
      use PolyORB.Exceptions;
      use PolyORB.Types;

      package PATC renames PolyORB.Any.TypeCode;

      Name : constant RepositoryId := Occurrence_To_Name (E);
      TC : PATC.Object := PATC.TC_Except;
      Result : PolyORB.Any.Any;
   begin
      --  Name
      PATC.Add_Parameter
        (TC, To_Any (PolyORB.Types.String (Name)));

      --  RepositoryId : 'DSA:<Name>:<version>'
      PATC.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("DSA:")
                       & PolyORB.Types.String (Name)
                       & PolyORB_Exc_Version));

      --  Valuation: Exception_Message

      PATC.Add_Parameter
        (TC, To_Any (TC_String));
      PATC.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("exception_message")));

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element
        (Result, To_Any
           (To_PolyORB_String (Ada.Exceptions.Exception_Message (E))));
      return Result;
   end DSA_Exception_To_Any;

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

   function FA_String (Item : PolyORB.Any.Any) return String is
   begin
      return PolyORB.Types.To_String (From_Any (Item));
   end FA_String;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID (Name : String) return RPC.Partition_ID
   is
      Info : constant RCI_Info := Retrieve_RCI_Info (Name);
   begin
      if Info.Is_Local then
         return Get_Local_Partition_ID;
      end if;

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
         PolyORB.Requests.Destroy_Request
           (Request);

         return RPC.Partition_ID (FA_I (Result.Argument));
      end;
   end Get_Active_Partition_ID;

   function Get_Aggregate_Element
     (Value : Any;
      Tc    : PolyORB.Any.TypeCode.Object;
      Index : System.Unsigned_Types.Long_Unsigned)
      return Any is
   begin
      return PolyORB.Any.Get_Aggregate_Element
        (Value, Tc, PolyORB.Types.Unsigned_Long (Index));
   end Get_Aggregate_Element;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   Local_Partition_ID : RPC.Partition_ID;
   Local_Partition_ID_Allocated : Boolean := False;

   function Get_Local_Partition_ID return RPC.Partition_ID is
   begin
      if not Local_Partition_ID_Allocated then
         Local_Partition_ID
           := RPC.Partition_ID
             (PolyORB.DSA_P.Partitions.Allocate_Partition_ID (""));
         --  XXX could set a useful partition name...
         Local_Partition_ID_Allocated := True;
      end if;
      return Local_Partition_ID;
   end Get_Local_Partition_ID;

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

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
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
            if Receiving_Stub (Self.Impl_Info.all).Kind = Pkg_Stub then

               --  The base reference for an RCI unit implements operations
               --  that correspond to the visible subprograms of the unit
               --  (which are handled by Self.Handler).
               --  In addition, it implements the following special operations:

               --  XXX these hand-crafted skels should be generated by
               --  auto-generated ones constructed from a distributed object
               --  type declaration.

               if PolyORB.Types.To_Standard_String (EMsg.Req.Operation)
                 = Op_Resolve
               then

                  -------------
                  -- resolve --
                  -------------

                  --  Resolve the name of a remote subprogram declared in
                  --  this remote call interface unit to the corresponding
                  --  reference, for the purpose of constructing a remote
                  --  access-to-subprogram value.

                  --  Code extracted from CosNaming::NamingContext
                  --  IDL skel.

                  declare
                     use CosNaming;
                     --  For exception and types.

                     n             : CosNaming.Name;
                     pragma Warnings (Off, n);
                     --  Accessed before it has a value (by To_Any).

                     Arg_Name_n  : constant PolyORB.Types.Identifier :=
                       To_PolyORB_String ("n");
                     Argument_n  : constant Any :=
                       CosNaming.Helper.To_Any (n);

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

                     declare
                        package ISNC renames
                          IDL_SEQUENCE_CosNaming_NameComponent;
                     begin
                        --  Convert arguments from their Any

                        n := CosNaming.Helper.From_Any (Argument_n);

                        --  Call implementation
                        Get_RAS_Ref
                          (Receiving_Stub (Self.Impl_Info.all).Name.all,
                           CosNaming.To_Standard_String
                             (ISNC.Element_Of (ISNC.Sequence (n), 1).id),
                              Result);

--                 exception
--                    when E : NotFound =>
--                       declare
--                          Members : NotFound_Members;
--                       begin
--                          Get_Members (E, Members);
--                          CORBA.ServerRequest.Set_Exception
--                            (EMsg.Req,
--                        CosNaming.NamingContext.Helper.To_Any (Members));
--                          return;
--                       end;
--                    when E : CannotProceed =>
--                       declare
--                          Members : CannotProceed_Members;
--                       begin
--                          Get_Members (E, Members);
--                          CORBA.ServerRequest.Set_Exception
--                            (EMsg.Req,
--                        CosNaming.NamingContext.Helper.To_Any (Members));
--                          return;
--                       end;
--                    when E : InvalidName =>
--                       declare
--                          Members : InvalidName_Members;
--                       begin
--                          Get_Members (E, Members);
--                          CORBA.ServerRequest.Set_Exception
--                            (Request,
--                         CosNaming.NamingContext.Helper.To_Any (Members));
--                          return;
--                       end;
                     end;

                     --  Set Result

                     EMsg.Req.Result :=
                       (Name      => PolyORB.Types.To_PolyORB_String
                        ("result"),
                        Arg_Modes => ARG_OUT,
                        Argument  => PolyORB.Any.ObjRef.To_Any (Result));
                  end;
                  goto Request_Completed;

               elsif PolyORB.Types.To_Standard_String (EMsg.Req.Operation)
                 = Op_Get_Partition_Id
               then

                  declare
                     Arg_List    : NVList_Ref;
                  begin

                     -----------------------
                     -- _get_partition_id --
                     -----------------------

                     --  Return the partition identifier assigned to
                     --  the partition on which this RCI unit resides.

                     NVList_Create (Arg_List);
                     Request_Arguments (EMsg.Req, Arg_List);
                     --  Must call Arguments (with an empty Arg_List)
                     --  to notify the protocol personality that this
                     --  request has been completely received.

                     EMsg.Req.Result :=
                       (Name      => PolyORB.Types.To_PolyORB_String
                          ("result"),
                        Arg_Modes => ARG_OUT,
                        Argument  => TA_I (Integer (Get_Local_Partition_ID)));
                     goto Request_Completed;
                  end;
               end if;
            end if;

            --  An actual user-defined subprogram:
            --  perform upcall to implementation.

            pragma Assert (Self.Handler /= null);
            declare
               use PolyORB.Exceptions;
            begin
               Self.Handler.all (EMsg.Req);
            exception
               when E : others =>
                  EMsg.Req.Exception_Info
                    := DSA_Exception_To_Any (E);
                  --  XXX Should map Ada exceptions to Anies
                  --  correctly!
            end;

            <<Request_Completed>>
            return Executed_Request'(Req => EMsg.Req);
         end;
      end if;
      return Result;
   end Execute_Servant;

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

      PolyORB.Setup.Proxies_POA (Root_POA_Object);

      pragma Debug (O ("Initializing DSA library units"));
      It := First (All_Receiving_Stubs);
      while not Last (It) loop
         declare
            use PolyORB.Obj_Adapters;
            use PolyORB.ORB;
            use PolyORB.Setup;

            Stub : Receiving_Stub renames Value (It).all;

         begin
            pragma Debug (O ("Setting up RPC receiver: " & Stub.Name.all));
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
                     pragma Debug
                       (O ("Registering local RCI: " & Stub.Name.all));
                     Known_RCIs.Register
                       (To_Lower (Stub.Name.all), RCI_Info'
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
                        --  Partition_IDs).
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

            Addr : System.Address := System.Null_Address;
            Receiver : Servant_Access := null;

         begin

            --  XXX
            --  The following is ugly and inefficient (two levels
            --  of linear search) and should probably be optimized
            --  in some way.

            pragma Debug (O ("Looking up RAS ref for " &
                               Subprogram_Name & " in " &
                               Pkg_Name));

            All_Stubs :
            while not Last (It) loop
               declare
                  S : Receiving_Stub renames Value (It).all;
                  pragma Assert (S.Subp_Info /= Null_Address);
                  subtype Subp_Info_T is RCI_Subp_Info_Array
                    (0 .. S.Subp_Info_Len - 1);
                  package Cvt is
                     new System.Address_To_Access_Conversions
                    (Subp_Info_T);
                  Subp_Info : constant Cvt.Object_Pointer
                    := Cvt.To_Pointer (S.Subp_Info);
               begin
                  if S.Kind = Pkg_Stub and then S.Name.all = Pkg_Name then
                     pragma Debug (O ("Found package!"));
                     for J in Subp_Info'Range loop
                        declare
                           Info : RCI_Subp_Info
                             renames Subp_Info (J);

                           subtype Fixed_Str is
                             String (1 .. Info.Name_Length);

                           package Str_Addr_Conversion is
                              new System.Address_To_Access_Conversions
                             (Fixed_Str);
                           use Str_Addr_Conversion;
                        begin
                           if To_Pointer (Info.Name).all
                             = Subprogram_Name
                           then
                              pragma Debug (O ("Found subprogram!"));
                              Addr := Info.Addr;
                              Receiver := S.Receiver;
                              exit All_Stubs;
                           else
                              pragma Debug (O ("Skipping subprogram: "
                                & To_Pointer (Info.Name).all));
                              null;
                           end if;
                        end;
                     end loop;
                  end if;
               end;
               Next (It);
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
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;

   begin
      if Last in Typ'Range and then Typ (Last) = ASCII.NUL then
         Last := Last - 1;
      end if;

      if Addr /= Null_Address then
         pragma Assert (Receiver.Object_Adapter /= null);

         declare
            Key : aliased PolyORB.Objects.Object_Id
              := To_Local_Oid (Addr);

            Oid : aliased PolyORB.Objects.Object_Id
              := PolyORB.Obj_Adapters.Export
              (OA  => Receiver.Object_Adapter,
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
      PolyORB.Tasking.Soft_Links.Enter_Critical_Section;
      Answer := Objects_HTable.Get (Handler);
      if Answer = null then
         Answer := new RACW_Stub_Type;

         --  We leak memory here each time we receive a new
         --  unique value of a remote access to classwide or
         --  remote access to subprogram type.

         Answer.Origin   := Handler.Origin;
         Answer.Receiver := Handler.Receiver;
         Answer.Target   := Handler.Target;
         Answer.Addr     := Handler.Addr;
         Answer.Asynchronous := Handler.Asynchronous;

         Objects_HTable.Set (Answer, Answer);
      else
         PolyORB.Smart_Pointers.Dec_Usage (Handler.Target);
      end if;
      Handler := Answer;
      PolyORB.Tasking.Soft_Links.Leave_Critical_Section;
   end Get_Unique_Remote_Pointer;

   ------------------------------
   -- Request_Raise_Occurrence --
   ------------------------------

   procedure Request_Raise_Occurrence (R : Request_Access) is
      use Ada.Exceptions;
   begin
      PolyORB.Exceptions.Default_Raise_From_Any (R.Exception_Info);

   end Request_Raise_Occurrence;

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
            Subp_Info           => Null_Address,
            Subp_Info_Len       => 0,
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
   end Register_Pkg_Receiving_Stub;

   -----------------------
   -- Retrieve_RCI_Info --
   -----------------------

   function Retrieve_RCI_Info (Name : String) return RCI_Info
   is
      LName : constant String := To_Lower (Name);
      Info : RCI_Info;
      pragma Warnings (Off, Info);
      --  The default initialization value is meaningful.
   begin
      pragma Debug (O ("Retrieve RCI info: " & Name));
      Info := Known_RCIs.Lookup (LName, Info);
      if PolyORB.References.Is_Nil (Info.Base_Ref) then
         --  Not known yet: we therefore know that it is remote,
         --  and that we need to look it up with the naming service.
         Info := RCI_Info'
           (Base_Ref => CORBA.Object.To_PolyORB_Ref
              (PolyORB.CORBA_P.Naming_Tools.Locate
                 (LName & ".RCI")),
            Is_Local => False,
            Is_All_Calls_Remote => True);
         Known_RCIs.Register (LName, Info);
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
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;

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

      POA.Default_Servant := PolyORB.Servants.Servant_Access
        (Default_Servant);

      Default_Servant.Object_Adapter :=
        PolyORB.Obj_Adapters.Obj_Adapter_Access (POA);
      pragma Assert (Default_Servant.Object_Adapter /= null);

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
         & "corba.initial_references"
         & "poa_config.racws?"
         & "CosNaming.BindingIterator.Helper"
         & "CosNaming.Helper"
         & "CosNaming.NamingContext.Helper"
         & "tcp_access_points.soap?"
         & "tcp_access_points.corba?"
         & "tcp_access_points.srp?",
       Provides => Empty,
       Init => Initialize'Access));

   --  XXX should depend on virtual module 'access_points' only.

end System.PolyORB_Interface;
