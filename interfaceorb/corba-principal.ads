package CORBA.Principal is

   type Object is private;

   function To_Any(From : in Object) return Any;
   function From_Any(From : in Any ) return Object;

   function Is_Principal(Item : Any) return Boolean;

   -- other implementation dependant operations

end CORBA.Principal;

