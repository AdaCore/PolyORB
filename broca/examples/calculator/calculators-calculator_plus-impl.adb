----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA; use CORBA;
with calculators.calculator_plus.Skel;

package body calculators.calculator_plus.Impl is


   function add
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return (Left_Op1 + Right_Op2);
   end add;


   function subtract
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return (Left_Op1 - Right_Op2);
   end subtract;


   function multiply
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return (Left_Op1 * Right_Op2);
   end multiply;


   function divide
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return (Left_Op1 / Right_Op2);
   end divide;

end calculators.calculator_plus.Impl;
