with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;

with PolyORB.References;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;

--  XXX the following are dependant on configuration options
--  and should be moved to a generated unit (à la s-garela).
--  (an OA is required only on server units; any OA is OK
--  for RCIs without RACWs, but RACWs require a POA).

with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config.Minimum;

package body System.PolyORB_Interface is

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("system.polyorb_interface");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


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
            --  XXX register ref with naming service so it
            --  can be located by other partitions.

            Free (Stub.Name);
            Free (Stub.Version);
         end;
         Next (It);
      end loop;
      Deallocate (All_Receiving_Stubs);
      pragma Debug (O ("Done initializing DSA."));
   end Initialize;

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
       Depends => +"orb" & "access_points",
       Provides => Empty,
       Init => Initialize'Access));
end System.PolyORB_Interface;
