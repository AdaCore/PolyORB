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

--  This package contains an interface for manipulation of task specific data

package System.Task_Specific_Data is

   --  Here we define a single type that encapsulates the various task
   --  specific data. This type is used to store the necessary data into
   --  the Task_Control_Block. This record could use detailed comments ???

   type TSD is
      record
         Jmpbuf_Address : Address := Null_Address;
         GNAT_Exception : Address := Null_Address;
         Sec_Stack_Addr : Address := Null_Address;
         Exc_Stack_Addr : Address := Null_Address;
         Message_Length : Integer := 0;
         Message_Addr   : Address := Null_Address;
      end record;

   --  This package just re-exports the Get/Set routines for the various
   --  Task Specific Data (TSD) elements from System.Tasking_Soft_Links,
   --  but as callable subprograms instead of objects of access to
   --  subprogram types.

   function  Get_Jmpbuf_Address return  Address;
   procedure Set_Jmpbuf_Address (Addr : Address);
   pragma Inline (Get_Jmpbuf_Address);
   pragma Inline (Set_Jmpbuf_Address);
   --  These routines provide a task specific address used to store the
   --  address of the current longjmp/setjmp jump buffer for exception
   --  management (under the current scheme which uses longjmp/setjmp)

   function  Get_GNAT_Exception return  Address;
   procedure Set_GNAT_Exception (Addr : Address);
   pragma Inline (Get_GNAT_Exception);
   pragma Inline (Set_GNAT_Exception);
   --  These routines provide a task specific address used to temporarily
   --  store the address of the current exception during propagation.

   function  Get_Sec_Stack_Addr return  Address;
   procedure Set_Sec_Stack_Addr (Addr : Address);
   pragma Inline (Get_Sec_Stack_Addr);
   pragma Inline (Set_Sec_Stack_Addr);
   --  These routines provide a task specific address used to reference
   --  the currently allocated secondary stack.

   function  Get_Exc_Stack_Addr return Address;
   procedure Set_Exc_Stack_Addr (Addr : Address);
   pragma Inline (Get_Exc_Stack_Addr);
   pragma Inline (Set_Exc_Stack_Addr);
   --  These routines provide the address of a task-specific stack used
   --  for the propagation of exceptions in response to synchronous faults.
   --  This alternate stack is necessary when propagating Storage_Error
   --  resulting from a stack overflow, as the task's primary stack is
   --  exhausted. Non-SGI platforms may implement these as dummy bodies.

   function  Get_Message_Length return Integer;
   procedure Set_Message_Length (Len : Integer);
   pragma Inline (Get_Message_Length);
   pragma Inline (Set_Message_Length);
   --  These routines provide access to the length of the currently raised
   --  exception's message. O means no message. Negative values have a
   --  special purpose in connection with Assert_Failure (see description
   --  in System.Assertions (s-assert.adb) for explanation). If a negative
   --  value is set using Set_Message_Length, then a subsequent call to
   --  Set_Message_Length with an argument of zero will, instead of setting
   --  the length to zero, change the sign of the previously set negative
   --  length.

   function  Get_Message_Addr return Address;
   procedure Set_Message_Addr (Addr : Address);
   pragma Inline (Get_Message_Addr);
   pragma Inline (Set_Message_Addr);
   --  These routines provide access to the buffer containing the currently
   --  raised exception's message. The actual message is:
   --    To_Big_String_Ptr (Get_Message_Addr).all (1 .. Get_Message_Length)

   procedure Create_TSD (New_TSD : in out TSD);
   --  Called from s-tassta when a new thread is created to perform
   --  any required initialization of the TSD.

   procedure Destroy_TSD (Old_TSD : in out TSD);
   --  Called from s-tassta  just before a thread is destroyed to perform
   --  any required finalization.

end System.Task_Specific_Data;
