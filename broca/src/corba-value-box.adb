------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      C O R B A . V A L U E . B O X                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;
with Ada.Unchecked_Deallocation;
with Broca.Exceptions;
with Broca.Refs;

package body CORBA.Value.Box is

   procedure FreeBox is
     new Ada.Unchecked_Deallocation (Boxed, Boxed_Access);

   --------------
   --  Create  --
   --------------
   function Create (With_Value : in Boxed) return Box_Ref is
      Result : Box_Ref;
      Ptr : Object_Ptr := new Object;
   begin
      Ptr.Content := new Boxed' (With_Value);
      Set (Result, CORBA.Impl.Object_Ptr (Ptr));
      return Result;
   end Create;

   ----------------
   --  Contents  --
   ----------------
   function Contents (The_Boxed : in Box_Ref)
     return Boxed_Access is
   begin
      if Is_Nil (The_Boxed) then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      return Object_Ptr (Object_Of (The_Boxed)).Content;
   end Contents;

   ---------------
   --  Release  --
   ---------------
   procedure Release (The_Ref : in out Box_Ref) is
   begin
      if Is_Nil (The_Ref) then
         Broca.Exceptions.Raise_Bad_Param;
      else
         FreeBox (Object_Ptr (Object_Of (The_Ref)).Content);
         Broca.Refs.Set (Broca.Refs.Ref (The_Ref), null);
      end if;
   end Release;

end CORBA.Value.Box;






