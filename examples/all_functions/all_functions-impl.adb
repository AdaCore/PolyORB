with CORBA.Object.OmniORB;
with all_functions.Skel;
package body all_functions.Impl is 

   function Get_the_attribute
     (Self : access Object)
      return CORBA.Short
   is
   begin
      return Self.Attribute;
   end Get_the_attribute;

   procedure Set_the_attribute
     (Self : access Object;
      To   : in CORBA.Short)
   is
   begin
      Self.Attribute := To;
   end Set_the_attribute;

   function Get_the_readonly_attribute
     (Self : access Object)
      return CORBA.Short
   is
   begin
      return 18;
   end Get_the_readonly_attribute;

   procedure void_proc
     (Self : access Object)
   is
   begin 
      null;
   end void_proc;

   procedure in_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : in CORBA.Short)
   is
   begin 
      null;
   end in_proc;

   procedure out_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : out CORBA.Short;
      c : out CORBA.Short)
   is
   begin 
      A := 10 ;
      B := 11 ;
      C := 12 ;
   end out_proc;

   procedure inout_proc
     (Self : access Object;
      a : in out CORBA.Short;
      b : in out CORBA.Short)
   is
   begin 
      A := A+1 ;
      B := B+1 ;
   end inout_proc;

   procedure in_out_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : out CORBA.Short;
      d : out CORBA.Short)
   is
   begin 
      C := 3 ;
      D := 4 ;
   end in_out_proc;

   procedure in_inout_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in out CORBA.Short;
      c : in CORBA.Short;
      d : in out CORBA.Short)
   is
   begin 
      B := 36 ;
      D := 40 ;
   end in_inout_proc;

   procedure out_inout_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : in out CORBA.Short;
      c : in out CORBA.Short;
      d : out CORBA.Short)
   is
   begin 
      A:= 45 ;
      B := 46 ;
      C := 47 ;
      D := 48 ;
   end out_inout_proc;

   procedure in_out_inout_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : out CORBA.Short;
      c : in out CORBA.Short)
   is
   begin 
      B := -54 ;
      C := C + 1 ;
   end in_out_inout_proc;

   function void_fun
     (Self : access Object)
      return CORBA.Short
   is
   begin 
      return 3 ;
   end void_fun;

   function in_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : in CORBA.Short)
      return CORBA.Short
   is
   begin 
      return 7 ;
   end in_fun;

   procedure out_fun
     (Self : access Object;
      a : out CORBA.Short;
      b : out CORBA.Short;
      c : out CORBA.Short;
      Returns : out CORBA.Short)
   is
   begin 
      A := 5 ;
      B := 6 ;
      C := 7 ;
      Returns := 10 ;
   end out_fun;

   procedure inout_fun
     (Self : access Object;
      a : in out CORBA.Short;
      b : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
   begin 
      A := A + 1 ;
      B := B + 1 ;
      Returns := A + B ;
   end inout_fun;

   procedure in_out_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : out CORBA.Short;
      d : out CORBA.Short;
      Returns : out CORBA.Short)
   is
   begin 
      C := B ;
      D := A ;
      Returns := A + B ;
   end in_out_fun;

   procedure in_inout_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : in out CORBA.Short;
      c : in CORBA.Short;
      d : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
   begin 
      B := B + A ;
      D := D + C ;
      Returns := B + D ;
   end in_inout_fun;

   procedure out_inout_fun
     (Self : access Object;
      a : out CORBA.Short;
      b : in out CORBA.Short;
      c : in out CORBA.Short;
      d : out CORBA.Short;
      Returns : out CORBA.Short)
   is
   begin 
      A:= B ;
      B := B + 1 ;
      D := C ;
      C := C + 1 ;
      Returns := A+B+C+D +1;
   end out_inout_fun;

   procedure in_out_inout_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : out CORBA.Short;
      c : in out CORBA.Short;
      Returns : out CORBA.Short)
   is
   begin 
      B := A+1 ;
      C := A + C ;
      Returns := -1 ;
   end in_out_inout_fun;

   procedure oneway_void_proc
     (Self : access Object)
   is
   begin 
      null;
   end oneway_void_proc;

   procedure oneway_in_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : in CORBA.Short)
   is
   begin 
      null;
   end oneway_in_proc;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         all_functions.Repository_Id);
      -- Add user code *BELOW* this line
   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      AdaBroker.OmniORB.Adjust
        (AdaBroker.OmniORB.ImplObject (Self));
      -- Add user code *BELOW* this line
   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin
      -- Add user code *BEFORE* this line
      AdaBroker.OmniORB.Finalize
        (AdaBroker.OmniORB.ImplObject (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (all_functions.Repository_Id,
      all_functions.Nil_Ref,
      all_functions.Skel.Dispatch'Access);
end all_functions.Impl;
