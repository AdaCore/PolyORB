------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . T A S K _ S P E C I F I C _ D A T A             --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains an interface for manipulation of task specific data

with Ada.Exceptions;

package System.Task_Specific_Data is

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;
   subtype EO  is Ada.Exceptions.Exception_Occurrence;

   --  Here we define a single type that encapsulates the various task
   --  specific data. This type is used to store the necessary data into
   --  the Task_Control_Block.

   type TSD is
      record
         Jmpbuf_Address : Address := Null_Address;
         --  Address of jump buffer used to store the address of the
         --  current longjmp/setjmp buffer for exception management.
         --  These buffers are threaded into a stack, and the address
         --  here is the top of the stack. A null address means that
         --  no exception handler is currently active.

         Sec_Stack_Addr : Address := Null_Address;
         --  Address of currently allocated secondary stack

         Exc_Stack_Addr : Address := Null_Address;
         --  Address of a task-specific stack used for the propagation of
         --  exceptions in response to synchronous faults. This alternate
         --  stack is necessary when propagating Storage_Error resulting
         --  from a stack overflow, as the task's primary stack is full.
         --  This is currently only used on the SGI, and this value stays
         --  null on other platforms.

         Current_Excep : aliased EO;
         --  Exception occurrence that contains the information for the
         --  current exception. Note that any exception in the same task
         --  destroys this information, so the data in this variable must
         --  be copied out before another exception can occur.

      end record;

   --  This package just re-exports the Get/Set routines for the various
   --  Task Specific Data (TSD) elements from System.Tasking_Soft_Links,
   --  but as callable subprograms instead of objects of access to
   --  subprogram types.

   function  Get_Jmpbuf_Address return  Address;
   procedure Set_Jmpbuf_Address (Addr : Address);
   pragma Inline (Get_Jmpbuf_Address);
   pragma Inline (Set_Jmpbuf_Address);

   function  Get_Sec_Stack_Addr return  Address;
   procedure Set_Sec_Stack_Addr (Addr : Address);
   pragma Inline (Get_Sec_Stack_Addr);
   pragma Inline (Set_Sec_Stack_Addr);

   function  Get_Exc_Stack_Addr return Address;
   procedure Set_Exc_Stack_Addr (Addr : Address);
   pragma Inline (Get_Exc_Stack_Addr);
   pragma Inline (Set_Exc_Stack_Addr);

   function  Get_Current_Excep return EOA;
   pragma Inline (Get_Current_Excep);

   procedure Create_TSD (New_TSD : in out TSD);
   --  Called from s-tassta when a new thread is created to perform
   --  any required initialization of the TSD.

   procedure Destroy_TSD (Old_TSD : in out TSD);
   --  Called from s-tassta  just before a thread is destroyed to perform
   --  any required finalization.

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);
   --  This function obtains the Exception_Id from the Exception_Occurrence
   --  referenced by the Current_Excep field of the task specific data, i.e.
   --  the call is equivalent to Exception_Identity (Get_Current_Excep.all)

end System.Task_Specific_Data;
