--  $Id$

with Droopi.Smart_Pointers;

package CORBA.Impl is

   --  pragma Elaborate_Body;

   type Object is new Droopi.Smart_Pointers.Entity with null record;
   type Object_Ptr is access all Object'Class;

   --  procedure Inc_Usage (Obj : in Object_Ptr);
   --  procedure Dec_Usage (Obj : in out Object_Ptr);

end CORBA.Impl;
