with CORBA;
with CORBA.ORB;

with PortableServer.POA;

with CosNaming.BindingIterator;
with CosNaming.BindingIterator.Impl;
with CosNaming.NamingContext.Skel;
pragma Elaborate (CosNaming.NamingContext.Skel);

with GNAT.HTable;
with GNAT.Task_Lock; use GNAT.Task_Lock;

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

package body CosNaming.NamingContext.Impl is

   Null_NC : constant NameComponent
     := (Istring (Ada.Strings.Unbounded.Null_Unbounded_String),
         Istring (Ada.Strings.Unbounded.Null_Unbounded_String));

   subtype Hash_Header is Natural range 0 .. 30;

   function Hash  (F : String_Access) return Hash_Header;
   function Equal (F1, F2 : String_Access) return Boolean;

   package BOHT is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Bounded_Object_Ptr,
      No_Element => null,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);

   package Names renames CosNaming.IDL_SEQUENCE_CosNaming_NameComponent;

   function Encode
     (Ctx : NamingContext_Ptr;
      N   : NameComponent)
     return String;

   function Look_For_BO_In_NC
     (NC : NamingContext_Ptr;
      Id : String)
     return Bounded_Object_Ptr;

   procedure Append_BO_To_NC
     (NC  : in NamingContext_Ptr;
      Id  : in String;
      BT  : in BindingType;
      Obj : in CORBA.Object.Ref);

   procedure Parse
     (Self : access Object;
      N    : in     Name;
      Len  : in out Natural;
      Ctx  : out    NamingContext.Ref;
      NC   : out    NameComponent);

   function To_Name (NC : NameComponent) return Name;

   procedure Valid
     (NC     : NamingContext_Ptr;
      Locked : Boolean := False);

   procedure Free is
      new Ada.Unchecked_Deallocation (Bounded_Object, Bounded_Object_Ptr);

   Seed : NC_Id := (others => 'A');

   Root_POA : constant PortableServer.POA.Ref
     := PortableServer.POA.To_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.ObjectId (To_CORBA_String ("RootPOA"))));

   --------------
   -- Allocate --
   --------------

   function Allocate return NC_Id is
      N : Natural := NC_Id_Size;

   begin
      while N > 0 loop
         if Seed (N) /= 'Z' then
            Seed (N) := Character'Succ (Seed (N));
            exit;
         end if;
         N := N - 1;
      end loop;

      if N = 0 then
         raise Program_Error;
      end if;

      while N < NC_Id_Size loop
         N := N + 1;
         Seed (N) := 'A';
      end loop;

      return Seed;
   end Allocate;

   -------------
   -- To_Name --
   -------------

   function To_Name (NC : NameComponent) return Name is
   begin
      return Name (Names.To_Sequence ((1 => NC)));
   end To_Name;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (F : String_Access) return Hash_Header is
      N : Natural := 0;

   begin
      --  Add up characters of name, mod our table size

      for J in F'Range loop
         N := (N + Character'Pos (F (J))) mod (Hash_Header'Last + 1);
      end loop;

      return N;
   end Hash;

   ------------
   -- Encode --
   ------------

   function Encode
     (Ctx : NamingContext_Ptr;
      N   : NameComponent)
     return String
   is
      Len : Natural;
      NI  : Natural := Length (N.Id);
      NK  : Natural := Length (N.Kind);

   begin
      Len := NC_Id_Size + NI + 1 + NK + 1;

      declare
         BON : String (1 .. Len);

      begin
         BON (1 .. NC_Id_Size) := String (Ctx.Id);
         Len := NC_Id_Size;

         BON (Len + 1 .. Len + NI) := To_String (N.Id);
         Len := Len + NI;

         BON (Len + 1) := ' ';
         Len := Len + 1;

         BON (Len + 1 .. Len + NK) := To_String (N.Kind);
         Len := Len + NK;

         BON (Len + 1) := ';';

         return BON;
      end;
   end Encode;

   -----------------------
   -- Look_For_BO_In_NC --
   -----------------------

   function Look_For_BO_In_NC
     (NC : NamingContext_Ptr;
      Id : String)
     return Bounded_Object_Ptr is
   begin
      return BOHT.Get (Id'Unrestricted_Access);
   end Look_For_BO_In_NC;

   -----------
   -- Valid --
   -----------

   procedure Valid
     (NC     : NamingContext_Ptr;
      Locked : Boolean := False) is
   begin
      if NC = null then
         if Locked then
            Unlock;
         end if;
         raise CannotProceed;
      end if;
   end Valid;

   ---------------------
   -- Append_BO_To_NC --
   ---------------------

   procedure Append_BO_To_NC
     (NC  : in NamingContext_Ptr;
      Id  : in String;
      BT  : in BindingType;
      Obj : in CORBA.Object.Ref)
   is
      BO : Bounded_Object_Ptr := new Bounded_Object;

   begin
      Valid (NC, True);

      BOHT.Set (Id'Unrestricted_Access, BO);
      BO.BT  := BT;
      BO.Obj := Obj;
      BO.NC  := NC;
      if NC.Head = null then
         NC.Head := BO;
         NC.Tail := BO;

      else
         BO.Prev      := NC.Tail;
         BO.Prev.Next := BO;
         NC.Tail      := BO;
      end if;
   end Append_BO_To_NC;

   -----------------------
   -- Remove_BO_From_NC --
   -----------------------

   procedure Remove_BO_From_NC
     (NC : in     NamingContext_Ptr;
      BO : in out Bounded_Object_Ptr) is
   begin
      Valid (NC, True);

      if NC.Head = BO then
         NC.Head := BO.Next;
      end if;
      if NC.Tail = BO then
         NC.Tail := BO.Prev;
      end if;
      if BO.Next /= null then
         BO.Next.Prev := BO.Prev;
      end if;
      if BO.Prev /= null then
         BO.Prev.Next := BO.Next;
      end if;
      declare
         BON : String := Encode (NC, BO.BN);
      begin
         BOHT.Set (BON'Unrestricted_Access, null);
      end;
      Free (BO);
   end Remove_BO_From_NC;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self : access Object;
      N    : in     Name;
      Len  : in out Natural;
      Ctx  : out    NamingContext.Ref;
      NC   : out    NameComponent)
   is
      use Names;

      NCA : Element_Array := To_Element_Array (Sequence (N));

   begin
      Valid (Self.Self);

      Len := NCA'Length;
      if Len = 0 then
         raise InvalidName;

      elsif Len > 1 then
         declare
            Left_Name : Name;
            Left_Obj  : CORBA.Object.Ref;
         begin
            Left_Name := To_Sequence (NCA (NCA'First .. NCA'Last - 1));
            Left_Obj  := Resolve (Self, Left_Name);
            Ctx       := NamingContext.To_Ref  (Left_Obj);
         exception when CORBA.Bad_Param =>
            raise NotFound;
         end;
      end if;
      NC := NCA (NCA'Last);
   end Parse;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self : access Object;
      N    : in CosNaming.Name;
      Obj  : in CORBA.Object.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.Bind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : String := Encode (Self.Self, Last);

         begin
            Lock;
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               Unlock;
               raise AlreadyBound;
            end if;

            Append_BO_To_NC (Self.Self, BON, NObject, Obj);
            Unlock;
         end;
      end if;
   end Bind;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (Self : access Object;
      N    : in CosNaming.Name;
      Obj  : in CORBA.Object.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         Rebind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bounded_Object_Ptr;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);
            if BO = null then
               Unlock;
               raise NotFound;
            end if;

            if BO.BT /= NObject then
               Unlock;
               raise CannotProceed;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Append_BO_To_NC   (Self.Self, BON, NObject, Obj);
            Unlock;
         end;
      end if;
   end Rebind;

   ------------------
   -- Bind_Context --
   ------------------

   procedure Bind_Context
     (Self : access Object;
      N    : in CosNaming.Name;
      NC   : in CosNaming.NamingContext.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.Bind_Context (Ctx, To_Name (Last), NC);

      else
         declare
            BON : String := Encode (Self.Self, Last);

         begin
            Lock;
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               Unlock;
               raise AlreadyBound;
            end if;

            Append_BO_To_NC (Self.Self, BON, NContext, CORBA.Object.Ref (NC));
            Unlock;
         end;
      end if;
   end Bind_Context;

   --------------------
   -- Rebind_Context --
   --------------------

   procedure Rebind_Context
     (Self : access Object;
      N    : in CosNaming.Name;
      NC   : in CosNaming.NamingContext.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.Rebind_Context (Ctx, To_Name (Last), NC);

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bounded_Object_Ptr;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);
            if BO = null then
               Unlock;
               raise NotFound;
            end if;

            if BO.BT /= NContext then
               Unlock;
               raise CannotProceed;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Append_BO_To_NC (Self.Self, BON, NContext, CORBA.Object.Ref (NC));
            Unlock;
         end;
      end if;
   end Rebind_Context;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Self : access Object;
      N    : in CosNaming.Name)
     return CORBA.Object.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         return NamingContext.Resolve (Ctx, To_Name (Last));

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bounded_Object_Ptr;
            Obj : CORBA.Object.Ref;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               Unlock;
               raise NotFound;
            end if;

            Obj := BO.Obj;
            Unlock;
            return Obj;
         end;
      end if;
   end Resolve;

   ------------
   -- Unbind --
   ------------

   procedure Unbind
     (Self : access Object;
      N    : in CosNaming.Name)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         Unbind (Ctx, To_Name (Last));

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bounded_Object_Ptr;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               Unlock;
               raise NotFound;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Unlock;
         end;
      end if;
   end Unbind;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (Self : access Object)
     return CosNaming.NamingContext.Ref
   is
      Ctx : NamingContext_Ptr;
      Oid : PortableServer.ObjectId;

   begin
      Ctx      := new Object;
      Ctx.Self := Ctx;

      Oid := PortableServer.POA.Activate_Object
        (Root_POA, PortableServer.Servant (Ctx));

      return NamingContext.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (Root_POA, PortableServer.Servant (Ctx)));
   end New_Context;

   ----------------------
   -- Bind_New_Context --
   ----------------------

   function Bind_New_Context
     (Self : access Object;
      N    : in CosNaming.Name)
     return CosNaming.NamingContext.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Parse (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         return Bind_New_Context (Ctx, To_Name (Last));

      else
         Ctx := New_Context (Self);
         Bind_Context (Self, N, Ctx);
         return Ctx;
      end if;
   end Bind_New_Context;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object) is
   begin
      Valid (Self.Self);
      if Self.Head /= null then
         raise NotEmpty;
      end if;
   end Destroy;

   ----------
   -- List --
   ----------

   procedure List
     (Self     : access Object;
      How_Many : in CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      BI       : out CosNaming.BindingIterator_Forward.Ref)
   is
      use CosNaming.BindingIterator.Impl;

      Len  : Natural := 0;
      Size : constant Natural := Natural (How_Many);
      Head : Bounded_Object_Ptr;
      Iter : BindingIterator_Ptr;
      Oid  : PortableServer.ObjectId;

   begin
      Valid (Self.Self);

      Lock;

      Head := Self.Head;
      while Head /= null loop
         Len  := Len + 1;
         Head := Head.Next;
      end loop;

      Head := Self.Head;
      if Size <= Len then
         declare
            Table : Bindings.Element_Array (1 .. Size);
         begin
            for I in 1 .. Size loop
               Table (I) := (To_Name (Head.BN), Head.BT);
               Head := Head.Next;
            end loop;
            BL  := BindingList (Bindings.To_Sequence (Table));
            Len := Len - Size;
         end;
      end if;

      Iter       := new CosNaming.BindingIterator.Impl.Object;
      Iter.Index := 1;
      Iter.Table := new Bindings.Element_Array (1 .. Len);

      for I in Iter.Table'Range loop
         Iter.Table (I) := (To_Name (Head.BN), Head.BT);
         Head := Head.Next;
      end loop;

      Unlock;

      Oid := PortableServer.POA.Activate_Object
        (Root_POA, PortableServer.Servant (Iter));

       BI := BindingIterator.Convert_Forward.To_Forward
        (BindingIterator.To_Ref
         (PortableServer.POA.Servant_To_Reference
          (Root_POA, PortableServer.Servant (Iter))));
   end List;

end CosNaming.NamingContext.Impl;
