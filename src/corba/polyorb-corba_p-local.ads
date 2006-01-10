------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O R B A _ P . L O C A L                 --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA.Object;

with PolyORB.Smart_Pointers;

package PolyORB.CORBA_P.Local is

   type Local_Object_Base is
      abstract new PolyORB.Smart_Pointers.Entity with null record;

   type Local_Object_Base_Ref is access all Local_Object_Base'Class;

   function Is_A
     (Obj             : access Local_Object_Base;
      Logical_Type_Id : String)
     return Boolean
      is abstract;

   function Is_Local (Self : CORBA.Object.Ref'Class) return Boolean;
   --  True iff Self is a valid reference a local object.
   --  Raise CORBA.Inv_Objref if reference is null.

   function Is_CORBA_Local (Self : CORBA.Object.Ref'Class) return Boolean;
   --  True iff Self is a valid reference a CORBA personality local object.
   --  Raise CORBA.Inv_Objref if reference is null.

end PolyORB.CORBA_P.Local;
