with all_functions.Skeleton ;
with Text_Io ; use Text_Io ;
with Corba ; use Corba ;

package body all_functions.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   -- Get_the_attribute
   --------------------------
   function Get_the_attribute(Self : access Object) return Corba.Short is
   begin
      Put_Line("Get_the_attribute") ;
      return Self.all.Attrib ;
   end;


   -- Set_the_attribute
   --------------------------
   procedure Set_the_attribute(Self : access Object ; To : in Corba.Short) is
   begin
      Put_Line("Set_the_attribute to "
               & Short'Image(To)) ;
      Self.all.Attrib := To ;
   end;


   -- Get_the_readonly_attribute
   --------------------------
   function Get_the_readonly_attribute(Self : access Object) return Corba.Short is
   begin
      Put_Line("Get_the_readonly_attribute") ;
      return Corba.Short(18) ;
   end;


   procedure void_proc(Self : access Object) is
   begin
      Put_Line("void_proc") ;
   end;


   procedure in_proc(Self : access Object; a : in Corba.Short; b : in Corba.Short; c : in Corba.Short) is
   begin
      Put_Line("in_proc : "& Corba.Short'Image(a)
            & ", "
            & Corba.Short'Image(b)
            & ", "
            & Corba.Short'Image(c)) ;
   end;


   procedure out_proc(Self : access Object; a : out Corba.Short; b : out Corba.Short; c : out Corba.Short) is
   begin
      Put_Line("out_proc to 10, 11, 12") ;
      A := 10 ;
      B := 11 ;
      C := 12 ;
   end;


   procedure inout_proc(Self : access Object; a : in out Corba.Short; b : in out Corba.Short) is
   begin
      Put_Line("inout_proc") ;
      A := A+1 ;
      B := B+1 ;
   end;


   procedure in_out_proc(Self : access Object;
                         a : in Corba.Short;
                         b : in Corba.Short;
                         c : out Corba.Short;
                         d : out Corba.Short) is
   begin
      Put_Line("in_out_proc"
               & Corba.Short'Image(a)
               & ", "
               & Corba.Short'Image(b)) ;
      C := 3 ;
      D := 4 ;
   end;


   procedure in_inout_proc(Self : access Object; a : in Corba.Short;
                                                 b : in out Corba.Short;
                                                 c : in Corba.Short;
                                                 d : in out Corba.Short) is
   begin
      Put_Line("in_inout_proc"
               & Corba.Short'Image(a)
               & ", "
               & Corba.Short'Image(b)
               & ", "
               & Corba.Short'Image(c)
               & ", "
               & Corba.Short'Image(d)) ;
      B := 36 ;
      D := 40 ;
   end;


   procedure out_inout_proc(Self : access Object; a : out Corba.Short; b : in out Corba.Short; c : in out Corba.Short; d : out Corba.Short) is
   begin
      Put_Line("out_inout_proc") ;
      A:= 45 ;
      B := 46 ;
      C := 47 ;
      D := 48 ;
   end;


   procedure in_out_inout_proc(Self : access Object;
                               a : in Corba.Short;
                               b : out Corba.Short;
                               c : in out Corba.Short) is
   begin
      Put_Line("in_out_inout_proc") ;
      B := -54 ;
      C := C + 1 ;
   end;


   function void_fun(Self : access Object) return Corba.Short is
   begin
      Put_Line("void_fun") ;
      return 3 ;
   end ;


   function in_fun(Self : access Object; a : in Corba.Short; b : in Corba.Short; c : in Corba.Short) return Corba.Short is
   begin
      Put_Line("in_fun") ;
      return 7 ;
   end ;


   procedure out_fun(Self : access Object;
                     a : out Corba.Short;
                     b : out Corba.Short;
                     c : out Corba.Short;
                     Returns : out Corba.Short) is
   begin
      Put_Line("out_fun") ;
      A := 5 ;
      B := 6 ;
      C := 7 ;
      Returns := 10 ;
   end;


   procedure inout_fun(Self : access Object;
                       a : in out Corba.Short;
                       b : in out Corba.Short;
                       Returns : out Corba.Short) is
   begin
      Put_Line("inout_fun") ;
      A := A + 1 ;
      B := B + 1 ;
      Returns := A + B ;
   end;


   procedure in_out_fun(Self : access Object;
                        a : in Corba.Short;
                        b : in Corba.Short;
                        c : out Corba.Short;
                        d : out Corba.Short;
                        Returns : out Corba.Short) is
   begin
      Put_Line("in_out_fun") ;
      C := B ;
      D := A ;
      Returns := A + B ;
   end;


   procedure in_inout_fun(Self : access Object;
                          a : in Corba.Short;
                          b : in out Corba.Short;
                          c : in Corba.Short;
                          d : in out Corba.Short;
                          Returns : out Corba.Short) is
   begin
      Put_Line("in_inout_fun") ;
      B := B + A ;
      D := D + C ;
      Returns := B + D ;
   end;


   procedure out_inout_fun(Self : access Object;
                           a : out Corba.Short;
                           b : in out Corba.Short;
                           c : in out Corba.Short;
                           d : out Corba.Short;
                           Returns : out Corba.Short) is
   begin
      Put_Line("out_inout_fun") ;
      A:= B ;
      B := B + 1 ;
      D := C ;
      C := C + 1 ;
      Returns := A+B+C+D +1;
   end;


   procedure in_out_inout_fun(Self : access Object;
                              a : in Corba.Short;
                              b : out Corba.Short;
                              c : in out Corba.Short;
                              Returns : out Corba.Short) is
   begin
      Put_Line("in_out_inout_fun") ;
      B := A+1 ;
      C := A + C ;
      Returns := -1 ;
   end;


   procedure Oneway_Void_Proc(Self : access Object) is
   begin
      Put_Line("Oneway_Void_Proc") ;
   end ;


   procedure Oneway_In_Proc(Self : access Object ;
                            A : in Corba.Short ;
                            B : in Corba.Short ;
                            C : in Corba.Short) is
   begin
      Put_Line("Oneway_In_Proc "
               & Corba.Short'Image(A)
               & " ,"
               & Corba.Short'Image(B)
               & " ,"
               & Corba.Short'Image(C)) ;
   end ;




   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize(AdaBroker.OmniORB.ImplObject(Self)) ;
      Initialize_Local_Object(Self,
                        Repository_Id,
                        all_functions.Skeleton.Dispatch'Access);
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   AdaBroker.OmniORB.Adjust(AdaBroker.OmniORB.ImplObject(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   AdaBroker.OmniORB.Finalize(AdaBroker.OmniORB.ImplObject(Self)) ;
   end Finalize ;


end all_functions.Impl ;
