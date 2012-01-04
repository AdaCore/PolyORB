------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O R B A _ P . L O C A L                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA;

with PolyORB.References;

package body PolyORB.CORBA_P.Local is

   --------------------
   -- Is_CORBA_Local --
   --------------------

   function Is_CORBA_Local (Self : CORBA.Object.Ref'Class) return Boolean is
   begin
      if PolyORB.Smart_Pointers.Is_Nil (PolyORB.Smart_Pointers.Ref (Self)) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return CORBA.Object.Entity_Of (Self).all
        in Local_Object_Base'Class;
   end Is_CORBA_Local;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Self : CORBA.Object.Ref'Class) return Boolean is
   begin
      if PolyORB.Smart_Pointers.Is_Nil (PolyORB.Smart_Pointers.Ref (Self)) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return not PolyORB.References.Is_Exported_Reference
        (CORBA.Object.Internals.To_PolyORB_Ref (CORBA.Object.Ref (Self)));
   end Is_Local;

end PolyORB.CORBA_P.Local;
