----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with PortableServer;

package calculators.simple_calculator.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function add
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long;

   function subtract
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long;

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end calculators.simple_calculator.Impl;
