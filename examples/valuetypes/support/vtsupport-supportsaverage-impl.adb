----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with VTsupport.SupportsAverage.Skel;

with Ada.Text_Io; use Ada.Text_IO;

package body VTsupport.SupportsAverage.Impl is


   function get_average
     (Self : access Object)
     return CORBA.Double
   is
      Result : CORBA.Double;
   begin
      Put_Line ("Average invocation on SupportsAverage interface");
      Result := CORBA.Double (-1);
      return Result;
   end get_average;

end VTsupport.SupportsAverage.Impl;
