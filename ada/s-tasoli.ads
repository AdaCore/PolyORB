------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . T A S K I N G _ S O F T _ L I N K S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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

--  This package contains a set of subprogram access variables that access
--  some basic tasking primitives that are called from non-tasking code (e.g.
--  the defer/undefer abort that surrounds a finalization action). To avoid
--  dragging in the tasking all the time, we use a system of soft links where
--  the links are initialized to dummy non-tasking versions, and then if the
--  tasking is initialized, they are reset to the real tasking versions.

package System.Tasking_Soft_Links is

   pragma Elaborate_Body;

   --  First we have the access subprogram types used to establish the links.
   --  The approach is to establish variables containing access subprogram
   --  values which by default point to dummy no tasking versions of routines.

   type No_Param_Proc    is access procedure;
   type Addr_Param_Proc  is access procedure (Addr : Address);

   type Get_Address_Call is access function return Address;
   type Set_Address_Call is access procedure (Addr : Address);

   type Get_Integer_Call is access function return Integer;
   type Set_Integer_Call is access procedure (Len : Integer);

   --  Declarations for the no tasking versions of the required routines

   procedure Abort_Defer_NT;
   --  Defer task abortion (non-tasking case, does nothing)

   procedure Abort_Handler_NT;
   --  Handle task abortion (non-tasking case, does nothing). Currently,
   --  only VMS uses this.

   procedure Abort_Undefer_NT;
   --  Undefer task abortion (non-tasking case, does nothing)

   function  Check_Abort_Status_NT return Integer;
   --  Returns Boolean'Pos (True) iff abort signal should raise
   --  Standard.Abort_Signal.

   procedure Task_Lock_NT;
   --  Lock out other tasks (non-tasking case, does nothing)

   procedure Task_Unlock_NT;
   --  Release lock set by Task_Lock (non-tasking case, does nothing)

   Abort_Defer : No_Param_Proc := Abort_Defer_NT'Access;
   --  Defer task abortion (task/non-task case as appropriate)

   Abort_Handler : No_Param_Proc := Abort_Handler_NT'Access;
   --  Handle task abortion (task/non-task case as appropriate)

   Abort_Undefer : No_Param_Proc := Abort_Undefer_NT'Access;
   --  Undefer task abortion (task/non-task case as appropriate)

   Check_Abort_Status : Get_Integer_Call := Check_Abort_Status_NT'Access;
   --  Called when Abort_Signal is delivered to the process.  Checks to
   --  see if signal should result in raising Standard.Abort_Signal.

   Lock_Task : No_Param_Proc := Task_Lock_NT'Access;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   Unlock_Task : No_Param_Proc := Task_Unlock_NT'Access;
   --  Releases lock previously set by call to Lock_Task. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.

   function  Get_Jmpbuf_Address_NT return  Address;
   procedure Set_Jmpbuf_Address_NT (Addr : Address);

   Get_Jmpbuf_Address : Get_Address_Call := Get_Jmpbuf_Address_NT'Access;
   Set_Jmpbuf_Address : Set_Address_Call := Set_Jmpbuf_Address_NT'Access;

   function  Get_GNAT_Exception_NT return  Address;
   procedure Set_GNAT_Exception_NT (Addr : Address);

   Get_Gnat_Exception : Get_Address_Call := Get_GNAT_Exception_NT'Access;
   Set_Gnat_Exception : Set_Address_Call := Set_GNAT_Exception_NT'Access;

   function  Get_Sec_Stack_Addr_NT return  Address;
   procedure Set_Sec_Stack_Addr_NT (Addr : Address);

   Get_Sec_Stack_Addr : Get_Address_Call := Get_Sec_Stack_Addr_NT'Access;
   Set_Sec_Stack_Addr : Set_Address_Call := Set_Sec_Stack_Addr_NT'Access;

   function  Get_Exc_Stack_Addr_NT return Address;
   procedure Set_Exc_Stack_Addr_NT (Addr : Address);

   Get_Exc_Stack_Addr : Get_Address_Call := Get_Exc_Stack_Addr_NT'Access;
   Set_Exc_Stack_Addr : Set_Address_Call := Set_Exc_Stack_Addr_NT'Access;


   function  Get_Message_Length_NT return Integer;
   procedure Set_Message_Length_NT (Len : Integer);

   Get_Message_Length : Get_Integer_Call := Get_Message_Length_NT'Access;
   Set_Message_Length : Set_Integer_Call := Set_Message_Length_NT'Access;

   function  Get_Message_Addr_NT return Address;
   procedure Set_Message_Addr_NT (Addr : Address);

   Get_Message_Addr : Get_Address_Call := Get_Message_Addr_NT'Access;
   Set_Message_Addr : Set_Address_Call := Set_Message_Addr_NT'Access;

   --------------------------------
   -- Secondary Stack Soft-Links --
   --------------------------------

   --  The inclusion of these soft-links in the Tasking_Soft_Links package is
   --  somewhat of a misnomer. The SS_Init/SS_Free mechanism is independent
   --  of the use of tasking. If no unit uses the secondary stack, the SS_Init
   --  and SS_Free links will default to a dummy implementation.  Otherwise,
   --  the elaboration of System.Secondary_Stack will establish links to the
   --  appropriate routines.

   procedure SS_Init_NT (Stk : out Address; Size : Natural);
   --  Initialization of the secondary stack (if no sec-stack does nothing)

   procedure SS_Free_NT (Stk : Address);
   --  Release the secondary stack (if no sec-stack does nothing)

   type Proc_SS1 is access procedure (X : Address);
   type Proc_SS2 is access procedure (X : out Address; Y : Natural);

   SS_Init : Proc_SS2 := SS_Init_NT'Access;
   SS_Free : Proc_SS1 := SS_Free_NT'Access;

end System.Tasking_Soft_Links;
