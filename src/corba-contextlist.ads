------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   C O R B A . C O N T E X T L I S T                      --
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

with CORBA.AbstractBase;
with CORBA.Impl;
with Sequences.Unbounded;
pragma Elaborate (Sequences.Unbounded);

package CORBA.ContextList is

   pragma Elaborate_Body;

   type Ref is new CORBA.AbstractBase.Ref with null record;
   Nil_Ref : constant Ref;

   type Object is new CORBA.Impl.Object with private;
   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);

   function Get_Count
     (Self : in Ref)
     return CORBA.Unsigned_Long;

   procedure Add
     (Self : in Ref;
      Exc : in CORBA.String);

   function Item
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
     return CORBA.String;

   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long);

   function Create_Object return Object_Ptr;

private
   --  The actual implementation of an ExceptionList:
   --  a list of CORBA.String
   package Context_Sequence is new Sequences.Unbounded (CORBA.String);

   type Object is new CORBA.Impl.Object with record
     List : Context_Sequence.Sequence := Context_Sequence.Null_Sequence;
   end record;

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Ref with null record);
end CORBA.ContextList;
