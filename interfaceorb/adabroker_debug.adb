with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body Adabroker_Debug is

   Filename : constant String := "adabroker_debug_options.txt";

   type Cell;
   type Cell_Ptr is access Cell;

   type Cell (N : Natural) is record
      Flag : String (1 .. N);
      Next : Cell_Ptr;
   end record;

   procedure Free_Cell is new Ada.Unchecked_Deallocation (Cell, Cell_Ptr);

   Flag_List : Cell_Ptr := null;

   procedure Add_To_Flag_List (S : in String);

   ----------------------
   -- Add_To_Flag_List --
   ----------------------

   procedure Add_To_Flag_List
     (S : in String)
   is
      Tmp : Cell_Ptr;
   begin
      Tmp       := new Cell (S'Length);
      Tmp.Flag  := S;
      Tmp.Next  := Flag_List;
      Flag_List := Tmp;
   end Add_To_Flag_List;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Flag : in String)
      return Boolean
   is
      Tmp : Cell_Ptr := Flag_List;
   begin
      while Tmp /= null loop
         if Tmp.Flag = Flag then
            return True;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return False;
   end Is_Active;

   ------------
   -- Output --
   ------------

   procedure Output
     (Flag : in Boolean;
      Msg  : in String)
   is
   begin
      if Flag then
         Ada.Text_IO.Put_Line (Msg);
      end if;
   end Output;

   File : Ada.Text_IO.File_Type;
   Line : String (1 .. 256);
   Last : Natural;

begin
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line, Last);
         if Last /= 0 then
            if Line (1) /= '#' then
               Add_To_Flag_List (Line (1 .. Last));
            end if;
         end if;
      end loop;

      Ada.Text_IO.Close (File);

   exception when others =>
      pragma Debug
        (Ada.Text_IO.Put_Line
         ("** No debugging options file : " & Filename & " **"));
      null;
   end;
end Adabroker_Debug;




