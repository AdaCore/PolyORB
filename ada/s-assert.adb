------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . A S S E R T I O N S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
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

--  Note: it would be much simpler to use Ada.Exceptions.Raise_Exception,
--  but that would drag in a lot of the runtime, and especially in the
--  compiler itself, which uses assertions, we do not want these extra
--  units included indirectly as a result of using assertions. The result
--  is that we duplicate subtantial chunks of Ada.Exceptions in this unit.

with System;
with System.Task_Specific_Data; use System.Task_Specific_Data;
with System.Standard_Library;
with Unchecked_Conversion;

package body System.Assertions is

   type Buffer_Ptr is access System.Standard_Library.Exception_Message_Buffer;
   --  A thin pointer to String

   function To_Buffer_Ptr is
     new Unchecked_Conversion (System.Address, Buffer_Ptr);
   --  Conversion from address to string access for exception msg manipulation

   --------------------------
   -- Raise_Assert_Failure --
   --------------------------

   procedure Raise_Assert_Failure (Msg : String) is
   begin
      --  Set message in place with negative length. There is a special
      --  arrangement with regard to negative lengths. When a negative
      --  length is set, then the next call to set the length to zero
      --  (which will happen when the exception is raised using the
      --  normal assert statement), results in negating the length so
      --  it now has the proper positive value.

      Set_Message_Length (-Msg'Length);
      To_Buffer_Ptr (Get_Message_Addr).all (1 .. Msg'Length) := Msg;

      --  The following is for back compatibility with the bootstrap
      --  path, and can be removed later on.

      Assert_Msg (1 .. Msg'Length) := Msg;
      Assert_Msg_Length := Msg'Length;

      --  Now raise the actual exception. This looks like a raise with no
      --  message, but because of the special handling for negative lengths
      --  as set above, this acts as a raise with message.

      raise Assert_Failure;

   end Raise_Assert_Failure;

end System.Assertions;
