------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System.Task_Specific_Data;
with System.Tasking_Soft_Links;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Secondary_Stack is

   use type SSE.Storage_Offset;

   --                                      +------------------+
   --                                      |       Next       |
   --                                      +------------------+
   --                                      |                  | Last (200)
   --                                      |                  |
   --                                      |                  |
   --                                      |                  |
   --                                      |                  |
   --                                      |                  |
   --                                      |                  | First (101)
   --                                      +------------------+
   --                         +----------> |          |       |
   --                         |            +----------+-------+
   --                         |                    |  |
   --                         |                    ^  V
   --                         |                    |  |
   --                         |            +-------+----------+
   --                         |            |       |          |
   --                         |            +------------------+
   --                         |            |                  | Last (100)
   --                         |            |         C        |
   --                         |            |         H        |
   --    +-----------------+  |  +-------->|         U        |
   --    |  Current_Chunk -|--+  |         |         N        |
   --    +-----------------+     |         |         K        |
   --    |       Top      -|-----+         |                  | First (1)
   --    +-----------------+               +------------------+
   --    | Default_Size    |               |       Prev       |
   --    +-----------------+               +------------------+
   --
   --
   type Memory is array (Mark_Id range <>) of SSE.Storage_Element;

   type Chunk_Id (First, Last : Mark_Id);
   type Chunk_Ptr is access Chunk_Id;

   type Chunk_Id (First, Last : Mark_Id) is record
      Prev, Next : Chunk_Ptr;
      Mem        : Memory (First .. Last);
   end record;

   type Stack_Id is record
      Top           : Mark_Id;
      Current_Chunk : Chunk_Ptr;
      Default_Size  : SSE.Storage_Count;
   end record;

   type Stack_Ptr is access Stack_Id;

   function From_Addr is new Unchecked_Conversion (Address, Stack_Ptr);
   function To_Addr   is new Unchecked_Conversion (Stack_Ptr, System.Address);

   --------------
   -- Allocate --
   --------------

   procedure SS_Allocate
     (Address      : out System.Address;
      Storage_Size : SSE.Storage_Count)
   is
      Stack        : constant Stack_Ptr :=
                       From_Addr
                         (System.Task_Specific_Data.Get_Sec_Stack_Addr);
      Chunk        : Chunk_Ptr := Stack.Current_Chunk;
      Max_Align    : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Max_Size     : constant Mark_Id :=
                       ((Mark_Id (Storage_Size) + Max_Align - 1) / Max_Align)
                         * Max_Align;

   begin
      --  The Current_Chunk may not be the good one if a lot of release
      --  operations have taken place. So go down the stack if necessary

      while  Chunk.First > Stack.Top loop
         Chunk := Chunk.Prev;
      end loop;

      --  Find out if the available memory in the current chunk is sufficient.
      --  if not, go to the next one and eventally create the necessary room

      while Chunk.Last - Stack.Top + 1 < Max_Size loop
         if Chunk.Next /= null then
            null;

         --  Create new chunk of the default size unless it is not sufficient

         elsif SSE.Storage_Count (Max_Size) <= Stack.Default_Size then
            Chunk.Next := new Chunk_Id (
              First => Chunk.Last + 1,
              Last  => Chunk.Last + Mark_Id (Stack.Default_Size));

            Chunk.Next.Prev := Chunk;

         else
            Chunk.Next := new Chunk_Id (
              First => Chunk.Last + 1,
              Last  => Chunk.Last + Max_Size);

            Chunk.Next.Prev := Chunk;
         end if;

         Chunk     := Chunk.Next;
         Stack.Top := Chunk.First;
      end loop;

      --  Resulting address is the address pointed by Stack.Top

      Address      := Chunk.Mem (Stack.Top)'Address;
      Stack.Top    := Stack.Top + Mark_Id (Max_Size);
      Stack.Current_Chunk := Chunk;
   end SS_Allocate;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init (Stk : out System.Address; Size : Natural) is
      Stack : Stack_Ptr;

   begin
      Stack               := new Stack_Id;
      Stack.Current_Chunk := new Chunk_Id (1, Mark_Id (Size));
      Stack.Top           := 1;
      Stack.Default_Size  := SSE.Storage_Count (Size);

      Stk := To_Addr (Stack);
   end SS_Init;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stk : System.Address) is
      Stack : Stack_Ptr := From_Addr (Stk);
      Chunk : Chunk_Ptr := Stack.Current_Chunk;

      procedure Free is new Unchecked_Deallocation (Stack_Id, Stack_Ptr);
      procedure Free is new Unchecked_Deallocation (Chunk_Id, Chunk_Ptr);

   begin
      while Chunk.Prev /= null loop
         Chunk := Chunk.Prev;
      end loop;

      while Chunk.Next /= null loop
         Chunk := Chunk.Next;
         Free (Chunk.Prev);
      end loop;

      Free (Chunk);
      Free (Stack);
   end SS_Free;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return From_Addr (System.Task_Specific_Data.Get_Sec_Stack_Addr).Top;
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      From_Addr (System.Task_Specific_Data.Get_Sec_Stack_Addr).Top := M;
   end SS_Release;

   -------------
   -- SS_Info --
   -------------

   procedure SS_Info is
      Stack     : constant Stack_Ptr :=
                    From_Addr
                      (System.Task_Specific_Data.Get_Sec_Stack_Addr);
      Nb_Chunks : Integer            := 1;
      Chunk     : Chunk_Ptr          := Stack.Current_Chunk;

   begin
      Put_Line ("Secondary Stack information:");

      while Chunk.Prev /= null loop
         Chunk := Chunk.Prev;
      end loop;

      while Chunk.Next /= null loop
         Nb_Chunks := Nb_Chunks + 1;
         Chunk := Chunk.Next;
      end loop;

      --  Current Chunk information

      Put_Line (
        "  Total size              : "
        & Mark_Id'Image (Chunk.Last)
        & " bytes");
      Put_Line (
        "  Current allocated space : "
        & Mark_Id'Image (Stack.Top - 1)
        & " bytes");

      Put_Line (
        "  Number of Chunks       : "
        & Integer'Image (Nb_Chunks));

      Put_Line (
        "  Default size of Chunks : "
        & SSE.Storage_Count'Image (Stack.Default_Size));
   end SS_Info;

begin
   System.Tasking_Soft_Links.SS_Init := SS_Init'Access;
   System.Tasking_Soft_Links.SS_Free := SS_Free'Access;

   declare
      Main_SS : Address;

   begin
      --  What is this constant doing here ???
      SS_Init (Main_SS, 10 * 1024);
      System.Tasking_Soft_Links.Set_Sec_Stack_Addr_NT (Main_SS);
   end;

end System.Secondary_Stack;
