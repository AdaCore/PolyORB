--  The root Policy package.

--  $Id$

with CORBA.Policy_Types; use CORBA.Policy_Types;

with Sequences.Unbounded;

package CORBA.Policy is

   type Policy is abstract tagged null record;
   type Policy_Ptr is access all Policy;

   package Policy_Sequences is new Sequences.Unbounded (Policy_Ptr);
   subtype PolicyList is Policy_Sequences.Sequence;

end CORBA.Policy;
