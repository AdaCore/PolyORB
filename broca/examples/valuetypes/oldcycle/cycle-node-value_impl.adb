----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with CORBA.Impl;

with Ada.Text_Io; use Ada.Text_Io;

package body cycle.Node.Value_Impl is


   function get_number
     (Self : access Object)
     return CORBA.Long is
   begin
      return Self.all.number;
   end get_number;


   procedure set_number
     (Self : access Object;
      To : in CORBA.Long) is
   begin
      Self.all.number := To;
   end set_number;


   function get_next
     (Self : access Object)
     return cycle.Node.Value_Ref is
   begin
      return Self.all.next;
   end get_next;


   procedure set_next
     (Self : access Object;
      To : in cycle.Node.Value_Ref) is
   begin
      Self.all.next := To;
   end set_next;

   function createElement
     (l : in CORBA.Long)
      return Object_Ptr is
      Result : Object_Ptr := new Object;
   begin
      Result.Number := L;
      Result.Next := (CORBA.AbstractBase.Ref with null record);
      return Result;
      end createElement;

   function createList
     (l : in CORBA.Long;
      list : in cycle.Node.Value_Ref)
      return Object_Ptr is
      Result : Object_Ptr := new Object;
   begin
      Result.Number := L;
      Result.Next := List;
      return Result;
   end createList;


   function getPrevious
     (Self : access Object)
     return CORBA.Long
   is
   begin
      return CORBA.Long (-1);
   end getPrevious;


   function cmdLineManipulate
     (Self : access Object) return Cycle.Node.Value_Ref is
      Cmd : String(1..50);
      Last : Natural;
      Temp : Cycle.Node.Value_Ref;
   begin
      Cycle.Node.Set (Temp, CORBA.Impl.Object_Ptr (Self));
      loop
         Put_Line ("Current List:");
         Cycle.Node.Print (Temp);
         Put_Line("");
         Put_Line ("Write a number to append it, or 'X' to exit");
         Get_Line (Cmd, Last);
         exit when Cmd'Length > 0
           and then Cmd (1) = 'X';
         declare
            L : CORBA.Long;
         begin
            L := CORBA.Long'Value (Cmd (1..Last));
            Temp := Cycle.Node.Value_Ref (Cycle.Node.CreateList (L, Temp));
         exception
            when others =>
               Put_Line ("Illegal entry");
         end;
      end loop;
      return Temp;
   end cmdLineManipulate;


   procedure print
     (Self : access Object) is
   begin
      Put (CORBA.Long'Image (Self.Number) & " ");
      if not Cycle.Node.Is_Nil (Self.Next) then
         Cycle.Node.Print (Self.Next);
      end if;
   end print;

end cycle.Node.Value_Impl;
