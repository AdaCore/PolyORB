------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . I R _ T O O L S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2002 ENST Paris University, France.            --
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

--  $Id$

package body PolyORB.CORBA_P.IR_Tools is

   use CORBA.Repository_Root.Repository;

   Repo_Root_Ref : Ref;

   function Get_IR_Root
     return CORBA.Repository_Root.Repository.Ref is
   begin
      if Is_Nil (Repo_Root_Ref) then
         Repo_Root_Ref := Helper.To_Ref
           (PolyORB.CORBA_P.Naming_Tools.Locate
            ("Interface_Repository"));
      end if;
      return Repo_Root_Ref;
   end Get_IR_Root;

end PolyORB.CORBA_P.IRools;
