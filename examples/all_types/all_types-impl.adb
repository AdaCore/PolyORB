with all_types.Skeleton ;


package body all_types.Impl is


   procedure simple_exception_test(Self : access Object) is
   begin
      raise Simple_Exception ;
   end;


   procedure complexe_exception_test(Self : access Object) is
      Member : Complexe_Exception_Members ;
   begin
      Member.Excep := 21 ;
      Corba.Raise_Corba_Exception (Complexe_Exception'Identity,
                                   Member) ;
   end;


   function echo1(Self : access Object; arg : in example) return example is
   begin
      return Arg ;
   end ;


   function echo2(Self : access Object; arg : in simple_struct) return simple_struct is
   begin
      return Arg ;
   end ;


   function echo3(Self : access Object; arg : in Color) return Color is
   begin
      return Arg ;
   end ;


   function echo4(Self : access Object; arg : in U_string) return U_string is
   begin
      return Arg ;
   end ;


   function echo6(Self : access Object; arg : in U_sequence) return U_sequence is
   begin
      return Arg ;
   end ;


   function echo7(Self : access Object; arg : in B_sequence) return B_sequence is
   begin
      return Arg ;
   end ;


   -- Get_R_attribute
   --------------------------
   function Get_R_attribute(Self : access Object) return Color is
   begin
      return Self.all.Pd_Col ;
   end;


   -- Get_N_attribute
   --------------------------
   function Get_N_attribute(Self : access Object) return example is
   begin
      return Self.all.Pd_Ex ;
   end;


   -- Set_N_attribute
   --------------------------
   procedure Set_N_attribute(Self : access Object ; To : in example) is
   begin
      Self.all.Pd_Ex := To ;
   end;


   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Omniobject.Init_Local_Object(Omniobject.Implemented_Object(Self),
                                 Repository_Id, all_types.Skeleton.Dispatch'Access) ;
      -- You can add things *BELOW* this line
   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
      Omniobject.Adjust(Omniobject.Implemented_Object(Self)) ;
      -- You can add things *BELOW* this line
   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin
      -- You can add things *BEFORE* this line
      Omniobject.Finalize(Omniobject.Implemented_Object(Self)) ;
   end Finalize ;


end all_types.Impl ;
