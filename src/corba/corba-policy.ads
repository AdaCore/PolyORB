with CORBA.Policy_Types; use CORBA.Policy_Types;

package CORBA.Policy is

   --  The root Policy package.
   --  D�fini un type Policy abstrait, avec uniquement
   --
   --

   type Policy is abstract tagged null record;
   type Policy_Ptr is access all Policy;


end CORBA.Policy;
