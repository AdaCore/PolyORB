package body CORBA.AbstractBase is

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize (The_Ref : in out Ref) is
   begin
      null;
   end Initialize;

   -------------
   --  Adjust --
   -------------
   procedure Adjust (The_Ref : in out Ref) is
   begin
      if CORBA.Impl."/=" (The_Ref.Ptr, null) then
         CORBA.Impl.Inc_Usage (The_Ref.Ptr);
      end if;
   end Adjust;

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (The_Ref : in out Ref) is
   begin
      if CORBA.Impl."/=" (The_Ref.Ptr, null) then
         CORBA.Impl.Dec_Usage (The_Ref.Ptr);
      end if;
   end Finalize;

   -------------
   --  Is_Nil --
   -------------
   function Is_Nil  (Self : in Ref) return CORBA.Boolean is
   begin
      if CORBA.Impl."=" (Self.Ptr, null) then
         return True;
      else
         return False;
      end if;
   end Is_Nil;

   ---------------
   --  Release  --
   ---------------
   procedure Release (Self : in out Ref) is
   begin
      if CORBA.Impl."/=" (Self.Ptr, null) then
         CORBA.Impl.Dec_Usage (Self.Ptr);
         Self.Ptr := null;
      end if;
   end Release;

   ----------------
   --  Object_Of --
   ----------------
   function Object_Of (Self : Ref) return CORBA.Impl.Object_Ptr is
   begin
      return Self.Ptr;
   end Object_Of;

   ---------
   -- Get --
   ---------

   function Get (Self : in Ref) return CORBA.Impl.Object_Ptr is
   begin
      return Self.Ptr;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Ref;
                  Referenced : in CORBA.Impl.Object_Ptr) is
   begin
      if CORBA.Impl."/=" (Self.Ptr, null) then
         CORBA.Impl.Dec_Usage (Self.Ptr);
      end if;
      Self.Ptr := Referenced;
      if CORBA.Impl."/=" (Referenced, null) then
         CORBA.Impl.Inc_Usage (Referenced);
      end if;
   end Set;


end CORBA.AbstractBase;

