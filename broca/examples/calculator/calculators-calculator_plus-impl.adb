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


   function multiply
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Left_Op1 * Right_Op2;
   end multiply;


   function divide
     (Self : access Object;
      left_op1 : in CORBA.Long;
      right_op2 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Left_Op1 / Right_Op2;
   end divide;


   function add3
     (Self : access Object;
      op1 : in CORBA.Long;
      op2 : in CORBA.Long;
      op3 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Op1 + Op2 + Op3;
   end add3;


   function add4
     (Self : access Object;
      op1 : in CORBA.Long;
      op2 : in CORBA.Long;
      op3 : in CORBA.Long;
      op4 : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Op1 + Op2 + Op3 + Op4;
   end add4;


   function square
     (Self : access Object;
      Op : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return Op * Op;
   end square;

end calculators.calculator_plus.Impl;
