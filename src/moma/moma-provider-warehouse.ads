------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              M O M A . P R O V I D E R . W A R E H O U S E               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A dynamic, protected  dictionnary of Any, indexed by Strings.

--  $Id$

with PolyORB.Any;
with PolyORB.Utils.HTables.Perfect;
with PolyORB.Locks;

with MOMA.Types;

package MOMA.Provider.Warehouse is

   package Perfect_Htable is
      new PolyORB.Utils.HTables.Perfect (PolyORB.Any.Any);

   use Perfect_Htable;

   Key_Not_Found : exception;

   type Warehouse is private;

   procedure Register
     (W : in out Warehouse;
      K : String;
      V : PolyORB.Any.Any);
   --  Associate key K with value V.

   procedure Unregister
     (W : in out Warehouse;
      K : String);
   --  Remove any association for K. Key_Not_Found is raised
   --  if no value was registered for this key.

   function Lookup
      (W : Warehouse;
       K : String)
     return PolyORB.Any.Any;
   --  Lookup K in the dictionary, and return the associated value.
   --  Key_Not_Found is raised if no value was registered for this
   --  key.

   function Lookup
     (W : Warehouse;
      K : String;
      Default : PolyORB.Any.Any)
     return PolyORB.Any.Any;
   --  As above, but Default is returned for non-registered keys,
   --  insted of raising an exception.

   procedure Set_Persistence (W : in out Warehouse;
                              Persistence : MOMA.Types.Persistence_Mode);
   --  Set persistency flag for this warehouse,
   --  Note : this override any flag set for a message if set to a mode
   --  allowing persistence.

   --  XXX Warning : not safe in case of multiple message pools !!!!

private

   type Warehouse is record
      T             : Table_Instance;
      T_Initialized : Boolean := False;
      T_Persistence : MOMA.Types.Persistence_Mode := MOMA.Types.None;
      T_Lock        : PolyORB.Locks.Rw_Lock_Access;
   end record;

end MOMA.Provider.Warehouse;
