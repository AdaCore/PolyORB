----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA; use CORBA;
with calculators.simple_calculator.Skel;

package body calculators.simple_calculator.Impl is


   function add
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Left_Op1 + Right_Op2;
   end add;


   function subtract
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Left_Op1 - Right_Op2;
   end subtract;

end calculators.simple_calculator.Impl;
