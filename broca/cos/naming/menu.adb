with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Menu is

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   Args : array (1 .. 16) of String_Access;
   Argc : Natural;
   Line : String (1 .. 1024);
   Last : Natural;
   Scan : Natural;
   Pipe : Boolean := False;
   File : File_Type;

   function Next return String;

   --------------
   -- Argument --
   --------------

   function Argument (Index : Natural) return String_Access is
   begin
      if Index > Argc then
         raise Constraint_Error;
      end if;
      return Args (Index);
   end Argument;

   -----------
   -- Count --
   -----------

   function Count
     (Prompt : String := "> ")
     return Natural
   is
   begin
      Put (Prompt);
      begin
         Get_Line (Current_Input, Line, Last);
      exception when others =>
         Close (File);
         Set_Input (Standard_Input);
         Pipe := False;
         Get_Line (Current_Input, Line, Last);
      end;
      if Pipe then
         Put_Line (Line (1 .. Last));
      end if;
      Scan := 1;
      Argc := 0;
      loop
         declare
            Arg : String := Next;
         begin
            exit when Arg = "";
            Argc := Argc + 1;
            if Args (Argc) /= null then
               Free (Args (Argc));
            end if;
            Args (Argc) := new String'(Arg);
         end;
      end loop;
      return Argc;
   end Count;

   ----------
   -- Next --
   ----------

   function Next return String is
      use Ascii;
      F, L  : Natural;

   begin
      while Scan <= Last
        and then (Line (Scan) = ' ' or else Line (Scan) = HT)
      loop
         Scan := Scan + 1;
      end loop;

      if Scan > Last then
         return "";
      end if;

      if Line (Scan) = '"' then -- "
         Scan := Scan + 1;
         F    := Scan;

         while Scan <= Last loop
            if Line (Scan) = '"' then --  "
               L    := Scan - 1;
               Scan := Scan + 1;
               return Line (F .. L);

            elsif Line (Scan) = NUL then
               return "";

            end if;

            Scan := Scan + 1;
         end loop;
         return "";

      else
         F := Scan;
         while Scan <= Last
           and then Line (Scan) /= ' '
           and then Line (Scan) /= HT
         loop
            L    := Scan;
            Scan := Scan + 1;
         end loop;
         return Line (F .. L);
      end if;
   end Next;

   ---------------
   -- Set_Input --
   ---------------

   procedure Set_Input
     (Filename : in String_Access) is
   begin
      Open (File, In_File, Filename.all);
      Set_Input (File);
      Pipe := True;

   exception when others =>
      Put_Line ("no such file");
   end Set_Input;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : String_Access) is
   begin
      for I in S'Range loop
         if S (I) in 'A' .. 'Z' then
            S (I) := Character'Val (Character'Pos (S (I))
                                    - Character'Pos ('A')
                                    + Character'Pos ('a'));
         end if;
      end loop;
   end To_Lower;

end Menu;
