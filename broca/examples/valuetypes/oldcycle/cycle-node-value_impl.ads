----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with CORBA;
with CORBA.Value;

package cycle.Node.Value_Impl is

   type Object is new CORBA.Value.Impl_Base with record
      number : CORBA.Long;
      next : cycle.Node.Value_Ref;
   end record;

   type Object_Ptr is access all Object'Class;

   function get_number
     (Self : access Object)
     return CORBA.Long;

   procedure set_number
     (Self : access Object;
      To : in CORBA.Long);

   function get_next
     (Self : access Object)
     return cycle.Node.Value_Ref;

   procedure set_next
     (Self : access Object;
      To : in cycle.Node.Value_Ref);

   function createElement
     (l : in CORBA.Long)
      return Object_Ptr;

   function createList
     (l : in CORBA.Long;
      list : in cycle.Node.Value_Ref)
      return Object_Ptr;

   function getPrevious
     (Self : access Object)
     return CORBA.Long;

   function cmdLineManipulate
     (Self : access Object)
     return Cycle.Node.Value_Ref;

   procedure print
     (Self : access Object);

end cycle.Node.Value_Impl;
