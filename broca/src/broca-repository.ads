------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     B R O C A . R E P O S I T O R Y                      --
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

with CORBA;
with CORBA.Object;

package Broca.Repository is

   function Is_Equivalent
     (RI1 : CORBA.RepositoryId;
      RI2 : CORBA.RepositoryId)
     return Boolean;
   --  Return True if, and only if, RI1 and RI2 denote the same
   --  repository entity (a case-insensitive string match).

   --  The repository contains all the known factories.

   type Factory_Type;
   type Factory_Ptr is access Factory_Type'Class;
   type Factory_Type is abstract tagged
      record
         Next    : Factory_Ptr;
         Type_Id : CORBA.RepositoryId;
      end record;

   --  Primitive operation for a factory: create a new object.
   function Create
     (Factory : access Factory_Type)
     return CORBA.Object.Ref'Class is abstract;

   --  Add a new factory to the repository.
   procedure Register (Factory : in Factory_Ptr);

   --  Create an object from a type_id
   function Create
     (Type_Id : CORBA.RepositoryId)
     return CORBA.Object.Ref'Class;

end Broca.Repository;
