----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with TVTForward.second;
with CORBA;
with Ada.Text_Io; use Ada.Text_Io;
with CORBA.Impl;
with Tvtforward.Second.Value_Impl;

package body TVTForward.first.Value_Impl is


   function get_s
     (Self : access Object)
     return CORBA.String is
   begin
      return Self.all.s;
   end get_s;


   procedure set_s
     (Self : access Object;
      To : in CORBA.String) is
   begin
      Self.all.s := To;
   end set_s;


   function get_att
     (Self : access Object)
     return TVTForward.second.Value_Ref is
   begin
      return Self.all.att;
   end get_att;


   procedure set_att
     (Self : access Object;
      To : in TVTForward.second.Value_Ref) is
   begin
      Self.all.att := To;
   end set_att;

   function create
     (l : in CORBA.Long;
      s : in CORBA.String)
      return Object_Ptr is
      Result : Object_Ptr := new Object;
   Temp : Second.Value_Ref;
   F : Value_Ref;
   Sobj : CORBA.Impl.Object_Ptr
     := new Second.Value_Impl.Object;
   begin
      First.Set (F, CORBA.Impl.Object_Ptr (Result));
      Tvtforward.Second.Set (Temp, Sobj);

      Second.Set_L (Temp, L);
      Second.Set_Att (Temp,
                      Tvtforward.First.Convert_Forward.To_Forward (F));

      Set_S (Result, S);
      Set_Att (Result, Temp);

      return Result;
   end create;


   procedure print
     (Self : access Object) is
      use CORBA;
   begin
      Put_Line ("first : "
                & To_Standard_String (Get_S (Self)));
      Put_Line ("Second : "
                & Long'Image (Second.Get_L (Get_Att (Self))));
   end print;

end TVTForward.first.Value_Impl;
