with Ada.Characters.Handling;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.HTable;

with CORBA.Object;
with PolyORB.CORBA_P.Naming_Tools;
--  XXX while RCI initial refs are managed through the CORBA
--  naming service.

with PolyORB.Binding_Data;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.References;
with PolyORB.References.IOR;
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

   --  During elaboration, each RCI package on this partition is
   --  registered as a Receiving_Stub entry.
   --
   --  The All_Receiving_Stubs list is subsequently traversed
   --  during PolyORB's initialization, so the proper structures
   --  are registered with the middleware and the naming service.

   type Receiving_Stub is record
      Name     : String_Ptr;
      Receiver : PolyORB.Objects.Servant_Access;
      Version  : String_Ptr;
   end record;

   package Receiving_Stub_Lists is new PolyORB.Utils.Chained_Lists
     (Receiving_Stub);

   All_Receiving_Stubs : Receiving_Stub_Lists.List;

   procedure Initialize;
   --  Initialization procedure to be called during the
   --  global PolyORB initialization.

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
--     function To_Local_Oid is
--        new Ada.Unchecked_Conversion (System.Address, Local_Oid);
   function To_Address is
      new Ada.Unchecked_Conversion (Local_Oid, System.Address);

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
   begin
      pragma Assert (Self.Handler /= null);
      return Self.Handler.all (Msg);
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
            use PolyORB.Objects;
            use PolyORB.ORB;
            use PolyORB.Setup;

            Stub : Receiving_Stub := Value (It).all;

            Oid : aliased PolyORB.Objects.Object_Id
              := Export (Object_Adapter (The_ORB), Stub.Receiver);
            Ref : PolyORB.References.Ref;
         begin
            pragma Debug (O ("Registering RCI: " & Stub.Name.all));
            Create_Reference
              (The_ORB, Oid'Access,
               Stub.Name.all & ":" & Stub.Version.all,
               Ref);
            pragma Debug
              (O ("Done, ref is: " & PolyORB.References.Image (Ref)));
            pragma Debug
              (O ("Done, ref is: "
                    & PolyORB.Types.To_Standard_String
                    (PolyORB.References.IOR.Object_To_String (Ref))));

            declare
               CRef : CORBA.Object.Ref;
            begin
               CORBA.Object.Convert_To_CORBA_Ref (Ref, CRef);
               PolyORB.CORBA_P.Naming_Tools.Register
                 (Name => To_Lower (Stub.Name.all) & ".RCI",
                  Ref => CRef, Rebind => True);
               --  XXX Using the CORBA naming service is not necessarily
               --  a good idea. Alternative design: use a Boot_Server
               --  distributed object dedicated to DSA services.
            end;

            Free (Stub.Name);
            Free (Stub.Version);
         end;
         Next (It);
      end loop;
      Deallocate (All_Receiving_Stubs);
      pragma Debug (O ("Done initializing DSA."));
   end Initialize;

   --------------
   -- RCI_Info --
   --------------

   package body RCI_Info is

      Ref_Cache : PolyORB.References.Ref;

      function Get_RCI_Package_Ref
        return PolyORB.References.Ref is
      begin
         if PolyORB.References.Is_Nil (Ref_Cache) then
            Ref_Cache := CORBA.Object.To_PolyORB_Ref
              (PolyORB.CORBA_P.Naming_Tools.Locate
               (To_Lower (RCI_Name) & ".RCI"));
         end if;

         return Ref_Cache;
      end Get_RCI_Package_Ref;

   end RCI_Info;

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
         end if;
      end loop;

      Is_Local := False;
   end Get_Local_Address;

   -------------------
   -- Get_Reference --
   -------------------

   procedure Get_Reference
     (Addr     :        System.Address;
      Receiver : access Servant;
      Ref      :    out PolyORB.References.Ref)
   is
      Nil : PolyORB.References.Ref;
   begin
      Ref := Nil;
      --  XXX TBD;
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

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name     : in String;
      Receiver : in Servant_Access;
      Version  : in String := "")
   is
      use Receiving_Stub_Lists;
   begin
      Append
        (All_Receiving_Stubs,
         Receiving_Stub'
           (Name     => +Name,
            Receiver => Receiver,
            Version  => +Version));
   end Register_Receiving_Stub;

   --------------------------
   -- Setup_Object_Adapter --
   --------------------------

   function Setup_Object_Adapter
     (Name            : String;
      Configuration   : PolyORB.POA_Config.Configuration_Access;
      Default_Servant : Servant_Access)
     return PolyORB.Obj_Adapters.Obj_Adapter_Access
   is
      use PolyORB.POA;
      use PolyORB.POA_Config;
      use PolyORB.POA_Manager;

      POA : constant Obj_Adapter_Access
        := Create_POA
        (Self         => Root_POA_Object,
         Adapter_Name => PolyORB.Types.To_PolyORB_String (Name),
         A_POAManager => null,
         Policies     => Default_Policies (Configuration.all));
   begin
      POA.Default_Servant := Default_Servant;
      Activate (POAManager_Access (Entity_Of (POA.POA_Manager)));
      return PolyORB.Obj_Adapters.Obj_Adapter_Access (POA);
   end Setup_Object_Adapter;

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
       Depends => +"orb" & "access_points?" & "initial_references",
       Provides => Empty,
       Init => Initialize'Access));
end System.PolyORB_Interface;
