------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . A B S T R A C T B A S E              --
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

package CORBA.AbstractBase is

   type Ref is new Ada.Finalization.Controlled with
      record
         Ptr: CORBA.Impl.Object_Ptr := null;
      end record;

   procedure Initialize (The_Ref: in out Ref);
   procedure Adjust (The_Ref: in out Ref);
   procedure Finalize (The_Ref: in out Ref);

   procedure Unref (The_Ref: in out Ref)
     renames Finalize;

   function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   function Is_Null (Self : in Ref) return CORBA.Boolean
     renames Is_Nil;

   procedure Duplicate (Self : in out Ref)
     renames Adjust;

   procedure Release (Self : in out Ref);

   function Object_Of (Self : Ref) return CORBA.Impl.Object_Ptr;

end CORBA.AbstractBase;
