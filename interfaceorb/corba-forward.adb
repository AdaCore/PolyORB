------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . F O R W A R D                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.21 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

with CORBA.Object;

package body CORBA.Forward is

   -------------
   -- Convert --
   -------------

   package body Convert is

      ------------------
      -- From_Forward --
      ------------------

      function From_Forward
        (The_Forward : in Ref)
         return Ref_Type
      is
         Result : Ref_Type;
      begin
         CORBA.Object.Ref (Result) := CORBA.Object.Ref (The_Forward);
         return Result;
      end From_Forward;

      ----------------
      -- To_Forward --
      ----------------

      function To_Forward
        (The_Ref : in Ref_Type)
         return Ref
      is
         Result : Ref;
      begin
         CORBA.Object.Ref (Result) := CORBA.Object.Ref (The_Ref);
         return Result;
      end To_Forward;

   end Convert;

end CORBA.Forward;

