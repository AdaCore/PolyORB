------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . T A S K I N G _ S O F T _ L I N K S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

with System.Standard_Library;
with System.Task_Specific_Data;

package body System.Tasking_Soft_Links is

   --  Allocate an exception stack for the main program to use.
   --  We make sure that the stack has maximum alignment. Some systems require
   --  this (e.g. Sun), and in any case it is a good idea for efficiency.

   NT_Exc_Stack : array (0 .. 8192) of aliased Character;
   for NT_Exc_Stack'Alignment use Standard'Maximum_Alignment;

   NT_TSD : System.Task_Specific_Data.TSD;

   --------------------
   -- Abort_Defer_NT --
   --------------------

   procedure Abort_Defer_NT is
   begin
      null;
   end Abort_Defer_NT;

   --------------------
   -- Abort_Handler_NT --
   --------------------

   procedure Abort_Handler_NT is
   begin
      null;
   end Abort_Handler_NT;

   ----------------------
   -- Abort_Undefer_NT --
   ----------------------

   procedure Abort_Undefer_NT is
   begin
      null;
   end Abort_Undefer_NT;

   ---------------------------
   -- Check_Abort_Status_NT --
   ---------------------------

   function Check_Abort_Status_NT return Integer is
   begin
      return Boolean'Pos (False);
   end Check_Abort_Status_NT;

   ------------------------
   -- Complete_Master_NT --
   ------------------------

   procedure Complete_Master_NT is
   begin
      null;
   end Complete_Master_NT;

   -----------------------
   -- Current_Master_NT --
   -----------------------

   function Current_Master_NT return Integer is
   begin
      return 0;
   end Current_Master_NT;

   ---------------------
   -- Enter_Master_NT --
   ---------------------

   procedure Enter_Master_NT is
   begin
      null;
   end Enter_Master_NT;

   ------------------
   -- Task_Lock_NT --
   ------------------

   procedure Task_Lock_NT is
   begin
      null;
   end Task_Lock_NT;

   --------------------
   -- Task_Unlock_NT --
   --------------------

   procedure Task_Unlock_NT is
   begin
      null;
   end Task_Unlock_NT;

   ----------------
   -- SS_Init_NT --
   ----------------

   pragma Warnings (Off); --  since Stk not set by dummy routine

   procedure SS_Init_NT (Stk : out Address; Size : Natural) is
   begin
      null;
   end SS_Init_NT;

   pragma Warnings (On);

   ----------------
   -- SS_Free_NT --
   ----------------

   procedure SS_Free_NT (Stk : Address) is
   begin
      null;
   end SS_Free_NT;

   ---------------------------
   -- Get_Jmpbuf_Address_NT --
   ---------------------------

   function Get_Jmpbuf_Address_NT return  Address is
   begin
      return NT_TSD.Jmpbuf_Address;
   end Get_Jmpbuf_Address_NT;

   ---------------------------
   -- Set_Jmpbuf_Address_NT --
   ---------------------------

   procedure Set_Jmpbuf_Address_NT (Addr : Address) is
   begin
      NT_TSD.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address_NT;

   ---------------------------
   -- Get_Sec_Stack_Addr_NT --
   ---------------------------

   function Get_Sec_Stack_Addr_NT return  Address is
   begin
      return NT_TSD.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr_NT;

   ---------------------------
   -- Set_Sec_Stack_Addr_NT --
   ---------------------------

   procedure Set_Sec_Stack_Addr_NT (Addr : Address) is
   begin
      NT_TSD.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr_NT;

   ---------------------------
   -- Set_Sec_Stack_Addr_NT --
   ---------------------------

   function Get_Exc_Stack_Addr_NT return Address is
   begin
      return NT_TSD.Exc_Stack_Addr;
   end Get_Exc_Stack_Addr_NT;

   ---------------------------
   -- Set_Sec_Stack_Addr_NT --
   ---------------------------

   procedure Set_Exc_Stack_Addr_NT (Addr : Address) is
   begin
      NT_TSD.Exc_Stack_Addr := Addr;
   end Set_Exc_Stack_Addr_NT;

   --------------------------
   -- Get_Current_Excep_NT --
   --------------------------

   function Get_Current_Excep_NT return EOA is
   begin
      return NT_TSD.Current_Excep'Access;
   end Get_Current_Excep_NT;

begin
   NT_TSD.Exc_Stack_Addr := NT_Exc_Stack (8192)'Address;
end System.Tasking_Soft_Links;
