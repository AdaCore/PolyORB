package body CORBA.Impl is

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : in Object_Ptr) is
   begin
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Obj));
   end Inc_Usage;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage (Obj : in out Object_Ptr) is
   begin
      Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Obj));
   end Dec_Usage;

end CORBA.Impl;
