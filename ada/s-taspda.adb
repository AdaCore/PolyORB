-----------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . T A S K _ S P E C I F I C _ D A T A             --
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

with System.Standard_Library;

with System.Tasking_Soft_Links;
pragma Elaborate (System.Tasking_Soft_Links);

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Specific_Data is

   package SSL renames System.Standard_Library;
   package TSL renames System.Tasking_Soft_Links;

   type Buffer_Ptr is access SSL.Exception_Message_Buffer;
   --  A thin pointer to String

   function To_Address is
     new Unchecked_Conversion (Buffer_Ptr, Address);
   --  Conversion from string ptr to address for exception msg manipulation

   ------------------------
   -- Get_Jmpbuf_Address --
   ------------------------

   function Get_Jmpbuf_Address return  Address is
   begin
      return TSL.Get_Jmpbuf_Address.all;
   end Get_Jmpbuf_Address;

   ------------------------
   -- Set_Jmpbuf_Address --
   ------------------------

   procedure Set_Jmpbuf_Address (Addr : Address) is
   begin
      TSL.Set_Jmpbuf_Address (Addr);
   end Set_Jmpbuf_Address;

   ------------------------
   -- Get_GNAT_Exception --
   ------------------------

   function Get_GNAT_Exception return  Address is
   begin
      return TSL.Get_GNAT_Exception.all;
   end Get_GNAT_Exception;

   ------------------------
   -- Set_GNAT_Exception --
   ------------------------

   procedure Set_GNAT_Exception (Addr : Address) is
   begin
      TSL.Set_GNAT_Exception (Addr);
   end Set_GNAT_Exception;

   ------------------------
   -- Get_Sec_Stack_Addr --
   ------------------------

   function Get_Sec_Stack_Addr return  Address is
   begin
      return TSL.Get_Sec_Stack_Addr.all;
   end Get_Sec_Stack_Addr;

   ------------------------
   -- Set_Sec_Stack_Addr --
   ------------------------

   procedure Set_Sec_Stack_Addr (Addr : Address) is
   begin
      TSL.Set_Sec_Stack_Addr (Addr);
   end Set_Sec_Stack_Addr;

   ------------------------
   -- Get_Exc_Stack_Addr --
   ------------------------

   function Get_Exc_Stack_Addr return  Address is
   begin
      return TSL.Get_Exc_Stack_Addr.all;
   end Get_Exc_Stack_Addr;

   ------------------------
   -- Set_Exc_Stack_Addr --
   ------------------------

   procedure Set_Exc_Stack_Addr (Addr : Address) is
   begin
      TSL.Set_Exc_Stack_Addr (Addr);
   end Set_Exc_Stack_Addr;

   ------------------------
   -- Get_Message_Length --
   ------------------------

   function Get_Message_Length return Integer is
   begin
      return TSL.Get_Message_Length.all;
   end Get_Message_Length;


   ------------------------
   -- Set_Message_Length --
   ------------------------

   procedure Set_Message_Length (Len : Integer) is
   begin
      if Len = 0 then
         declare
            Old_Len : constant Integer := Get_Message_Length;

         begin
            if Old_Len < 0 then
               TSL.Set_Message_Length (-Old_Len);
               return;
            end if;
         end;
      end if;

      TSL.Set_Message_Length (Len);
   end Set_Message_Length;


   ----------------------
   -- Get_Message_Addr --
   ----------------------

   function Get_Message_Addr return Address is
   begin
      return TSL.Get_Message_Addr.all;
   end Get_Message_Addr;

   ----------------------
   -- Get_Message_Addr --
   ----------------------

   procedure Set_Message_Addr (Addr : Address) is
   begin
      TSL.Set_Message_Addr (Addr);
   end Set_Message_Addr;

   ----------------
   -- Create_TSD --
   ----------------

   procedure Create_TSD (New_TSD : in out TSD) is
      Ptr : constant Buffer_Ptr := new SSL.Exception_Message_Buffer;

      function To_Address is
        new Unchecked_Conversion (Buffer_Ptr, Address);

   begin
      --  Allocate 10K secondary stack

      TSL.SS_Init (New_TSD.Sec_Stack_Addr, 10*1024);

      --  Allocate a default buffer for the exeption message

      New_TSD.Message_Addr := To_Address (Ptr);
   end Create_TSD;

   -----------------
   -- Destroy_TSD --
   -----------------

   procedure Destroy_TSD (Old_TSD : in out TSD) is

      function To_Buffer_Ptr is
        new Unchecked_Conversion (Address, Buffer_Ptr);

      procedure Free is
         new Unchecked_Deallocation
           (SSL.Exception_Message_Buffer, Buffer_Ptr);

   begin
      TSL.SS_Free (Old_TSD.Sec_Stack_Addr);
      Free (To_Buffer_Ptr (Old_TSD.Message_Addr));
   end Destroy_TSD;

end System.Task_Specific_Data;
