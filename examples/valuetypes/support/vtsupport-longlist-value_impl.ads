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

package VTsupport.LongList.Value_Impl is

   type Object is new CORBA.Value.Impl_Base with record
      numbers : VTsupport.LongList.IDL_SEQUENCE_long.Sequence;
   end record;

   type Object_Ptr is access all Object'Class;

   function init

      return Object_Ptr;

   procedure addNumber
     (Self : access Object;
      n : in CORBA.Long);

   function get_average
     (Self : access Object)
     return CORBA.Double;

end VTsupport.LongList.Value_Impl;
