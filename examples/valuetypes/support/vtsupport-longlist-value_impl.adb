----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;

with Ada.Text_Io; use Ada.Text_Io;

with VTsupport.Longlist.Skel;



package body VTsupport.LongList.Value_Impl is

   function init

      return Object_Ptr is
      Result : Object_Ptr := new Object;
   begin
      Result.Numbers := VTsupport.Longlist.Idl_Sequence_Long.Null_Sequence;
      return Result;
   end init;


   procedure Print
     (Seq : in Idl_Sequence_Long.Sequence) is
   begin
      for I in 1 .. Idl_Sequence_Long.Length (Seq) loop
         Put (CORBA.Long'Image (Idl_Sequence_Long.Element_Of (Seq, I))
              & " ");
      end loop;
      Put_Line ("");
   end Print;

   procedure addNumber
     (Self : access Object;
      n : in CORBA.Long) is
   begin
      Idl_Sequence_Long.Append (Self.Numbers, N);
      Print (Self.Numbers);
   end addNumber;


   function get_average
     (Self : access Object)
      return CORBA.Double
   is
      Result : CORBA.Double;
      use CORBA;
   begin
      Put_Line ("***** VTsupport.Longlist.Value_Impl.get_average");
      for I in 1 .. Idl_Sequence_Long.Length (Self.Numbers) loop
         Result := Result +
           CORBA.Double (Idl_Sequence_Long.Element_Of (Self.Numbers, I));
      end loop;
      return Result / CORBA.Double (Idl_Sequence_Long.Length (Self.Numbers));
   end get_average;


end VTsupport.LongList.Value_Impl;
