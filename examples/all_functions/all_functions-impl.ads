with AdaBroker.OmniORB;
with CORBA; use CORBA;

package all_functions.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   function Get_the_attribute
     (Self : access Object)
      return CORBA.Short;

   procedure Set_the_attribute
     (Self : access Object;
      To   : in CORBA.Short);

   function Get_the_readonly_attribute
     (Self : access Object)
      return CORBA.Short;

   procedure void_proc
     (Self : access Object);

   procedure in_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : in CORBA.Short);

   procedure out_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : out CORBA.Short;
      c : out CORBA.Short);

   procedure inout_proc
     (Self : access Object;
      a : in out CORBA.Short;
      b : in out CORBA.Short);

   procedure in_out_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : out CORBA.Short;
      d : out CORBA.Short);

   procedure in_inout_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in out CORBA.Short;
      c : in CORBA.Short;
      d : in out CORBA.Short);

   procedure out_inout_proc
     (Self : access Object;
      a : out CORBA.Short;
      b : in out CORBA.Short;
      c : in out CORBA.Short;
      d : out CORBA.Short);

   procedure in_out_inout_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : out CORBA.Short;
      c : in out CORBA.Short);

   function void_fun
     (Self : access Object)
      return CORBA.Short;

   function in_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : in CORBA.Short)
      return CORBA.Short;

   procedure out_fun
     (Self : access Object;
      a : out CORBA.Short;
      b : out CORBA.Short;
      c : out CORBA.Short;
      Returns : out CORBA.Short);

   procedure inout_fun
     (Self : access Object;
      a : in out CORBA.Short;
      b : in out CORBA.Short;
      Returns : out CORBA.Short);

   procedure in_out_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : out CORBA.Short;
      d : out CORBA.Short;
      Returns : out CORBA.Short);

   procedure in_inout_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : in out CORBA.Short;
      c : in CORBA.Short;
      d : in out CORBA.Short;
      Returns : out CORBA.Short);

   procedure out_inout_fun
     (Self : access Object;
      a : out CORBA.Short;
      b : in out CORBA.Short;
      c : in out CORBA.Short;
      d : out CORBA.Short;
      Returns : out CORBA.Short);

   procedure in_out_inout_fun
     (Self : access Object;
      a : in CORBA.Short;
      b : out CORBA.Short;
      c : in out CORBA.Short;
      Returns : out CORBA.Short);

   procedure oneway_void_proc
     (Self : access Object);

   procedure oneway_in_proc
     (Self : access Object;
      a : in CORBA.Short;
      b : in CORBA.Short;
      c : in CORBA.Short);

private

   type Object is new AdaBroker.OmniORB.ImplObject with record
      Attribute : Corba.Short ;
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end all_functions.Impl;
