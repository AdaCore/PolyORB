----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with PortableServer;

package calculators.calculator_plus.Impl is

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

   function multiply
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long;

   function divide
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long;

   function add3
     (Self : access Object;
      op1 : in CORBA.Long;
      op2 : in CORBA.Long;
      op3 : in CORBA.Long)
     return CORBA.Long;

   function add4
     (Self : access Object;
      op1 : in CORBA.Long;
      op2 : in CORBA.Long;
      op3 : in CORBA.Long;
      op4 : in CORBA.Long)
     return CORBA.Long;

   function square
     (Self : access Object;
      op : in CORBA.Long)
     return CORBA.Long;

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end calculators.calculator_plus.Impl;
