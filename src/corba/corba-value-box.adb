------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C O R B A . V A L U E . B O X                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with CORBA.Impl;

package body CORBA.Value.Box is

   procedure FreeBox is
     new Ada.Unchecked_Deallocation (Boxed, Boxed_Access);

   --------------
   --  Create  --
   --------------

   function Create (With_Value : Boxed) return Box_Ref is
      Result : Box_Ref;
      Ptr    : constant Object_Ptr := new Object;

   begin
      Ptr.Content := new Boxed'(With_Value);
      Set (Result, CORBA.Impl.Object_Ptr (Ptr));

      return Result;
   end Create;

   ----------------
   --  Contents  --
   ----------------

   function Contents (The_Boxed : Box_Ref)
     return Boxed_Access
   is
   begin
      if Is_Nil (The_Boxed) then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      return Object_Ptr (Object_Of (The_Boxed)).Content;
   end Contents;

   ---------------
   --  Release  --
   ---------------

   procedure Release (The_Ref : in out Box_Ref) is
   begin
      if Is_Nil (The_Ref) then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);

      else
         FreeBox (Object_Ptr (Object_Of (The_Ref)).Content);
         Unref (The_Ref);
      end if;
   end Release;

end CORBA.Value.Box;
