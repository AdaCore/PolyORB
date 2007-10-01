------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . O R B _ I N I T              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines procedures to register call backs functions
--  to be called when initializing an ORB object.

package PolyORB.CORBA_P.ORB_Init is

   type ORB_Init_Suffix_Type is
     access function (Value : String) return Boolean;

   procedure Register
     (Suffix : String;
      ORB_Init_Suffix : ORB_Init_Suffix_Type);
   --  Attach ORB_Init_Suffix initialization routine to Suffix

   function Initialize (Suffix : String; Value : String) return Boolean;
   --  Initialize Suffix with Value. Return True if the initialisation
   --  was succesful, else return False.

   function Initialize (Value : String) return Boolean;
   --  If the N first characters of Value matches one registered
   --  suffix, run the corresponding initialization routine, else
   --  return false.

end PolyORB.CORBA_P.ORB_Init;
