------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           C O R B A . I M P L                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

--  with Ada.Finalization;
with Broca.Refs;

package CORBA.Impl is

   --  type Object is abstract new Ada.Finalization.Limited_Controlled
   --  with record
   --  COUNTER is used to count the number of references
   --  Counter : Integer := 0;
   --  end record;
   --  Counter : Integer := 0;
   --  end record;

   type Object is new Broca.Refs.Ref_Type with null record;
   type Object_Ptr is access all Object'Class;

   procedure Initialize (This : in out Object);
   procedure Finalize (This : in out Object);
   --  Adabroker specific  --
   procedure Inc_Usage (Obj : in Object_Ptr);
   procedure Dec_Usage (Obj : in out Object_Ptr);

end CORBA.Impl;
