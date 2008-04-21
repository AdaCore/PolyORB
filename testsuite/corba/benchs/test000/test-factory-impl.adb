------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T . F A C T O R Y . I M P L                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.ORB;

with Test_Support;

with Test.Factory.Skel;
pragma Warnings (Off, Test.Factory.Skel);

package body Test.Factory.Impl is

   -----------------------
   -- Create_References --
   -----------------------

   function Create_References
     (Self  : access Object;
      Count : CORBA.Long)
      return Test.Factory.EchoSequence
   is
      pragma Unreferenced (Self);

      Result : Test.Factory.EchoSequence;

   begin
      for J in 1 .. Natural (Count) loop
         Append (Result, Test_Support.To_Object_Reference (J));
      end loop;

      return Result;
   end Create_References;

   -----------------
   -- Preallocate --
   -----------------

   procedure Preallocate
     (Self  : access Object;
      Count : CORBA.Long)
   is
      pragma Unreferenced (Self);

   begin
      Test_Support.Preallocate (Natural (Count));
   end Preallocate;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Self : access Object) is
      pragma Unreferenced (Self);

   begin
      CORBA.ORB.Shutdown (False);
   end Shutdown;

end Test.Factory.Impl;
