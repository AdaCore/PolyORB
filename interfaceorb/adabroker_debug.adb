-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                package AdaBroker_Debug                        ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----    This is a debugging package for AdaBroker.                 ----
----    usage : add at the beginnig of each package :              ----
----                                                               ----
----    with Adabroker_Debug ;                                     ----
----    pragma Elaborate(Adabroker_Debug) ;                        ----
----                                                               ----
----   Debug : constant Boolean                                    ----
----           := Adabroker_Debug.Is_Active("specific_name") ;     ----
----                                                               ----
----   and then :                                                  ----
----                                                               ----
----     pragma Debug(Output(Debug, "degug info !!"))  ;           ----
----                                                               ----
----                                                               ----
----    The printing will be done if the Debug_Filename file       ----
----    contains a line with "specific_name"                       ----
----                                                               ----
----                                                               ----
----   authors : Fabien Azavant                                    ----
----   date    : 03/10/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Unchecked_Deallocation ;
with Text_Io ; use Text_Io ;

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


   -----------------------------
   -- reading the debug file  --
   -----------------------------
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




