with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Report;

procedure Test000 is

   use PolyORB.Utils.Report;

   package Ls is new PolyORB.Utils.Chained_Lists (Integer);
   use Ls;
   type A is array (Integer range <>) of Integer;

   function To_Array (L : List) return A;
   function To_Array (L : List) return A is
      Result : A (1 .. Length (L));
      Index : Integer := Result'First;
      It : Iterator := First (L);
   begin
      while not Last (It) loop
         Result (Index) := Value (It).all;
         Index := Index + 1;
         Next (It);
      end loop;

      if Index /= Result'Last + 1 then
         raise Program_Error;
      end if;
      return Result;
   end To_Array;

   procedure Show (LL : List);
   procedure Show (LL : List) is
      AA : constant A := To_Array (LL);
   begin
      for J in AA'Range loop
         Put (AA (J)'Img);
      end loop;
      New_Line;
   end Show;

   L1, L2, L3 : List;
   It : Iterator;
begin
   Output ("empty", To_Array (L1)'Length = 0);
   Append (L1, 123);
   Output ("single L1", To_Array (L1) = (1 => 123));
   Prepend (L2, 456);
   Output ("single L2", To_Array (L2) = (1 => 456));

   Prepend (L1, 666);
   Output ("double L1", To_Array (L1) = (1 => 666, 2 => 123));
   Append (L2, 789);
   Output ("double L1", To_Array (L2) = (1 => 456, 2 => 789));

   L3 := L1 & 999 & L2;
   Output ("concat", To_Array (L3) = (666, 123, 999, 456, 789));

   It := First (L3);
   while not (Last (It) or else Value (It).all = 999) loop
      Next (It);
   end loop;

   Output ("scan", Value (It).all = 999);
   Remove (L3, It);
   Output ("remove", To_Array (L3) = (666, 123, 456, 789));
   Output ("remove iterator", Value (It).all = 456);

   Insert (L3, 444, Before => It);
   Output ("insert", To_Array (L3) = (666, 123, 444, 456, 789));
   End_Report;
end Test000;
