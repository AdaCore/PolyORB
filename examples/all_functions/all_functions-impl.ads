with Omniobject ;
package all_functions.Impl is

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   function Get_the_attribute(Self : access Object) return Corba.Short ;

   procedure Set_the_attribute(Self : access Object ;
                               To : in Corba.Short ) ;

   function Get_the_readonly_attribute(Self : access Object) return Corba.Short ;

   procedure void_proc(Self : access Object ) ;
   procedure in_proc(Self : access Object; a : in Corba.Short; b : in Corba.Short; c : in Corba.Short ) ;
   procedure out_proc(Self : access Object; a : out Corba.Short; b : out Corba.Short; c : out Corba.Short ) ;
   procedure inout_proc(Self : access Object; a : in out Corba.Short; b : in out Corba.Short ) ;
   procedure in_out_proc(Self : access Object; a : in Corba.Short; b : in Corba.Short; c : out Corba.Short; d : out Corba.Short ) ;
   procedure in_inout_proc(Self : access Object; a : in Corba.Short; b : in out Corba.Short; c : in Corba.Short; d : in out Corba.Short ) ;
   procedure out_inout_proc(Self : access Object; a : out Corba.Short; b : in out Corba.Short; c : in out Corba.Short; d : out Corba.Short ) ;
   procedure in_out_inout_proc(Self : access Object; a : in Corba.Short; b : out Corba.Short; c : in out Corba.Short ) ;
   function void_fun(Self : access Object) return Corba.Short ;

   function in_fun(Self : access Object; a : in Corba.Short; b : in Corba.Short; c : in Corba.Short) return Corba.Short ;

   procedure out_fun(Self : access Object; a : out Corba.Short; b : out Corba.Short; c : out Corba.Short; Returns : out Corba.Short ) ;
   procedure inout_fun(Self : access Object; a : in out Corba.Short; b : in out Corba.Short; Returns : out Corba.Short ) ;
   procedure in_out_fun(Self : access Object; a : in Corba.Short; b : in Corba.Short; c : out Corba.Short; d : out Corba.Short; Returns : out Corba.Short ) ;
   procedure in_inout_fun(Self : access Object; a : in Corba.Short; b : in out Corba.Short; c : in Corba.Short; d : in out Corba.Short; Returns : out Corba.Short ) ;
   procedure out_inout_fun(Self : access Object; a : out Corba.Short; b : in out Corba.Short; c : in out Corba.Short; d : out Corba.Short; Returns : out Corba.Short ) ;
   procedure in_out_inout_fun(Self : access Object; a : in Corba.Short; b : out Corba.Short; c : in out Corba.Short; Returns : out Corba.Short ) ;

   ----   oneway   ----
   procedure Oneway_Void_Proc(Self : access Object) ;
   ----   oneway   ----
   procedure Oneway_In_Proc(Self : access Object ;
                            A : in Corba.Short ;
                            B : in Corba.Short ;
                            C : in Corba.Short) ;


private

   -- You may add fields to this record
   type Object is new Omniobject.Implemented_Object with record
      Attrib : Corba.Short ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end all_functions.Impl ;
