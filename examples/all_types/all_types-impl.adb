with all_types.Skeleton ;
with CORBA ;
with CORBA.Object ;


package body all_types.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   function echoBoolean(Self : access Object; arg : in CORBA.Boolean) return CORBA.Boolean is
   begin
      return Arg ;
   end ;


   function echoShort(Self : access Object; arg : in CORBA.Short) return CORBA.Short is
   begin
      return Arg ;
   end ;


   function echoLong(Self : access Object; arg : in CORBA.Long) return CORBA.Long is
   begin
      return Arg ;
   end ;


   function echoUShort(Self : access Object; arg : in CORBA.Unsigned_Short) return CORBA.Unsigned_Short is
   begin
      return Arg ;
   end ;


   function echoULong(Self : access Object; arg : in CORBA.Unsigned_Long) return CORBA.Unsigned_Long is
   begin
      return Arg ;
   end ;


   function echoFloat(Self : access Object; arg : in CORBA.Float) return CORBA.Float is
   begin
      return Arg ;
   end ;


   function echoDouble(Self : access Object; arg : in CORBA.Double) return CORBA.Double is
   begin
      return Arg ;
   end ;


   function echoChar(Self : access Object; arg : in CORBA.Char) return CORBA.Char is
   begin
      return Arg ;
   end ;


   function echoOctet(Self : access Object; arg : in CORBA.Octet) return CORBA.Octet is
   begin
      return Arg ;
   end ;


   function echoString(Self : access Object; arg : in CORBA.String) return CORBA.String is
   begin
      return Arg ;
   end ;


   procedure simple_exception_test(Self : access Object) is
   begin
      raise Simple_Exception ;
   end;


   procedure complexe_exception_test(Self : access Object) is
      Member : Complexe_Exception_Members ;
   begin
      Member.Excep := 21 ;
      CORBA.Raise_Corba_Exception (Complexe_Exception'Identity,
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


   function Get_R_attribute(Self : access Object) return Color is
   begin
      return Self.all.Pd_Col ;
   end ;


   function Get_N_attribute(Self : access Object) return example is
   begin
      return Self.all.Pd_Ex ;
   end ;


   procedure Set_N_attribute(Self : access Object ; To : in example) is
   begin
      Self.all.Pd_Ex := To ;
   end ;


   function echo8(Self : access Object; arg : in line) return line is
   begin
      return Arg ;
   end ;


   function echo9(Self : access Object; arg : in square) return square is
   begin
      return Arg ;
   end ;


   function echo10(Self : access Object; arg : in cube) return cube is
   begin
      return Arg ;
   end ;


   function echo11(Self : access Object; arg : in Ref) return Ref is
   begin
      return Arg ;
   end ;


   function echo12(Self : access Object; arg : in CORBA.Object.Ref) return CORBA.Object.Ref is
   begin
      return Arg ;
   end ;

   function Get_Myself(Self : access Object) return Ref is
   begin
      return To_Ref(Self.all) ;
   end ;




   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      AdaBroker.OmniObject.Initialize(AdaBroker.OmniObject.Implemented_Object(Self)) ;
      Init_Local_Object(Self,
                        Repository_Id,
                        all_types.Skeleton.Dispatch'Access,
                        all_types.Is_A'Access) ;
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   AdaBroker.OmniObject.Adjust(AdaBroker.OmniObject.Implemented_Object(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   AdaBroker.OmniObject.Finalize(AdaBroker.OmniObject.Implemented_Object(Self)) ;
   end Finalize ;


end all_types.Impl ;
