-----------------------------------------------------------------------
----                                                               ----
----                       AdaBroker                               ----
----                                                               ----
----    This is a debugging package for AdaBroker.                 ----
----                                                               ----
----                                                               ----
----                                                               ----
----                  package AdaBroker_Debug                      ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Text_Io ; use Text_Io ;
with Ada.Unchecked_Deallocation ;

package body Adabroker_Debug is

   -- procedures to handle the flag list
   type Cell ;
   type cell_Ptr is access Cell ;

   type Cell(N: Natural) is record
      Flag : String(1..N) ;
      Next : Cell_Ptr ;
   end record ;

   procedure Free_Cell is new Ada.Unchecked_Deallocation(Cell, Cell_Ptr) ;

   Flaglist : Cell_Ptr := null ;

   -- Add_To_Flag_List
   -------------------
   procedure Add_To_Flag_List(S : in String) is
      L : Natural ;
      Tmp : Cell_Ptr ;
   begin
      L := S'Length ;
      Tmp := new Cell(L) ;
      Tmp.Flag := S ;
      Tmp.Next := Flaglist ;
      Flaglist := Tmp ;
      Put_Line("adding " & Tmp.Flag) ;
   end ;

   -- Is_Active
   ------------
   function Is_Active (Flag : in String) return Boolean is
      Tmp : Cell_Ptr := Flaglist ;
   begin
      while not (Tmp = null ) loop
         if Tmp.Flag = Flag then
            return True ;
         end if ;
      end loop ;
      return False ;
   end ;


   -- Output
   ---------
   procedure Output (Flag : in Boolean ; Msg : in String ) is
   begin
      if Flag then
         Put_Line(Msg) ;
      end if ;
   end ;


   -- reading the debug file

   File : File_Type ;
   S : String(1..100) ;
   N : Natural ;
begin

   begin
   Open(File, In_File, Debug_Filename) ;
   exception
      when others =>
         Put_Line("Could not find file with debug options : " & Debug_Filename) ;
         raise Program_Error ;
   end ;

   while not End_Of_File(File) loop
      Get_Line(File, S, N) ;
      if not (N=0) then
         if not ( S(1) = '#' ) then
            Add_To_Flag_List(S(1..N)) ;
         end if ;
      end if ;
   end loop ;

   Close(File) ;

end Adabroker_Debug ;




