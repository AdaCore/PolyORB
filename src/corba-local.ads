package CORBA.Local is

   type Object is abstract new CORBA.Impl.Object with null record;
   --  ambiguous spec, does it inherit from nothing or from
   --  CORBA.Impl.Object ? CORBA.Impl.Object seems better, to provide
   --  reference counting.
   --  See mapping p1.27, that confirms this feeling

end CORBA.Local;
