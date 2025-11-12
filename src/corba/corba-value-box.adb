------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C O R B A . V A L U E . B O X                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

with CORBA.Impl;

package body CORBA.Value.Box is

   procedure FreeBox is
     new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Boxed,


      Name   => Boxed_Access);

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

   overriding procedure Release (The_Ref : in out Box_Ref) is
   begin
      if Is_Nil (The_Ref) then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);

      else
         FreeBox (Object_Ptr (Object_Of (The_Ref)).Content);
         Unref (The_Ref);
      end if;
   end Release;

end CORBA.Value.Box;
