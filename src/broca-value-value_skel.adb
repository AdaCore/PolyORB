------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . V A L U E . V A L U E _ S K E L                --
--                                                                          --
--                                 B o d y                                  --
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

with CORBA.Value;

with Broca.Names;
with Broca.Repository;

package body Broca.Value.Value_Skel is

   function Is_A
     (Type_Id : in Standard.String)
     return CORBA.Boolean;
   --  The Is_A operation for the root value type,
   --  CORBA::ValueBase.
   --  Actually, it should never be called since CORBA.Value.Impl_Base
   --  is abstract.

   ------------
   --  Is_A  --
   ------------
   function Is_A
     (Type_Id : in Standard.String)
     return CORBA.Boolean
   is
      use CORBA;
   begin
      return Broca.Repository.Is_Equivalent
        (Type_Id,
         Broca.Names.OMG_RepositoryId ("CORBA/ValueBase"));
   end Is_A;

begin

   Is_A_Store.Register_Operation
     (CORBA.Value.Impl_Base'Tag,
      Is_A'Access);

end Broca.Value.Value_Skel;
