----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with Ada.Text_Io; use Ada.Text_Io;
with CORBA.Impl;

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


   function get_previous
     (Self : access Object)
     return cycle.Node.Value_Ref is
   begin
      return Self.all.previous;
   end get_previous;


   procedure set_previous
     (Self : access Object;
      To : in cycle.Node.Value_Ref) is
   begin
      Self.all.previous := To;
   end set_previous;

   function createElement
     (l : in CORBA.Long)
      return Object_Ptr is
      Result : Object_Ptr := new Object;
   Ref : Cycle.Node.Value_Ref;
   begin
      Cycle.Node.Set (Ref, CORBA.Impl.Object_Ptr (Result));
      Result.Number := L;
      Result.Next := Ref;
      Result.Previous := Ref;
      return Result;
   end createElement;

   function createList
     (l : in CORBA.Long;
      list : in cycle.Node.Value_Ref)
      return Object_Ptr is
      Result : Object_Ptr := new Object;
   use Cycle.Node;
   Ref : Cycle.Node.Value_Ref;
   begin
      Cycle.Node.Set (Ref, CORBA.Impl.Object_Ptr (Result));
      Result.Next := List;
      Result.Previous := Get_Previous (List);
      Result.Number := L;
      Set_Previous (List, Ref);
      Set_Next (Result.Previous, Ref);
      return Result;
   end createList;


   function cmdLineManipulate
     (Self : access Object)
     return cycle.Node.Value_Ref
   is
      Cmd : String(1..50);
      Last : Natural;
      Temp : Cycle.Node.Value_Ref;
   begin
      Cycle.Node.Set (Temp, CORBA.Impl.Object_Ptr (Self));
      loop
         Put_Line ("Current List:");
         Cycle.Node.Print (Temp);
         Put_Line ("--- CMD ? (e(X)it, move (F)orward, move (B)ackward,"
                   & " (D)elete node, (R)evert list, or a number "
                   & "to add an element");
         Get_Line (Cmd, Last);
         if Last /= 0 then
            case Cmd (1) is
               when 'X'| 'x' =>
                  exit;
               when 'F' | 'f' =>
                  Temp := MoveForward (Temp);
               when 'B' | 'b' =>
                  Temp := MoveBackward (Temp);
               when 'D' | 'd' =>
                  Temp := RemoveNode (Temp);
               when 'R' | 'r' =>
                  Temp := Revert (Temp);
               when others =>
                  declare
                     L : CORBA.Long;
                  begin
                     L := CORBA.Long'Value (Cmd (1..Last));
                     Temp := Cycle.Node.Value_Ref (Cycle.Node.CreateList (L, Temp));
                  exception
                     when others =>
                        Put_Line ("Illegal entry");
                  end;
            end case;
         end if;
      end loop;
      return Temp;
   end cmdLineManipulate;


   procedure print
     (Self : access Object) is
      Start : Object_Ptr := Object_Ptr (Self);
      Current : Object_Ptr := Start;
   begin
      while True loop
         Put (CORBA.Long'Image (Current.Number) & " ");
         Current := Object_Ptr (Object_Of (Get_Next (Current)));
         exit when Current = Start;
      end loop;
      Put_Line ("");
   end print;


   function moveForward
     (Self : access Object)
     return cycle.Node.Value_Ref
   is
   begin
      return Get_Next (Self);
   end moveForward;


   function moveBackward
     (Self : access Object)
     return cycle.Node.Value_Ref
   is
   begin
      return Get_Previous (Self);
   end moveBackward;


   function revert
     (Self : access Object)
     return cycle.Node.Value_Ref
   is
      Start : Object_Ptr := Object_Ptr (Self);
      Current : Object_Ptr := Start;
      Result : cycle.Node.Value_Ref;
      Cur_Next : cycle.Node.Value_Ref;
   begin
      while True loop
         Cur_Next := Get_Next (Current);
         Set_Next (Current, Get_Previous (Current));
         Set_Previous (Current, Cur_Next);
         Current := Object_Ptr (Object_Of (Get_Next (Current)));
         exit when Current = Start;
      end loop;
      Set (Result, CORBA.Impl.Object_Ptr (Self));
      return Result;
   end revert;


   function removeNode
     (Self : access Object)
     return cycle.Node.Value_Ref
   is
      Result : cycle.Node.Value_Ref;
      use CORBA.Impl;
   begin
      Result := Get_Next (Self);
      if Object_Of (Result) = CORBA.Impl.Object_Ptr (Self) then
         Put_Line ("Cannot remove single node from List");
         Set (Result, CORBA.Impl.Object_Ptr (Self));
         return Result;
      else
         Set_Previous (Result, Get_Previous (Self));
         Set_Next (Get_Previous (Self), Result);
         return Result;
      end if;
   end removeNode;



end cycle.Node.Value_Impl;



