------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O R B A _ P . L O C A L                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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
