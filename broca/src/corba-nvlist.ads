------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . N V L I S T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with System;

with CORBA.AbstractBase;
with CORBA.Impl;

with Broca.Buffers;

with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);

package CORBA.NVList is

   type Ref is new CORBA.AbstractBase.Ref with null record;

   type Object is new CORBA.Impl.Object with private;
   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item_Type  : in CORBA.TypeCode.Object;
      Value      : in System.Address;
      Len        : in Long;
      Item_Flags : in Flags);

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags);

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue);

   --  Free and Free_Memory are no-ops in Ada.
   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref)
     renames Free;

   function Get_Count (Self : Ref) return CORBA.Long;

   -----------------------------------------
   -- The following is AdaBroker-specific --
   -----------------------------------------

   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data   : Ref);
   --   Marshall all the NamedValues of an NVList

   procedure Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data : in out Ref);
   --  Unmarshall all the NamedValues of an NVList

   function Create_Object return Object_Ptr;
   --  Create a new and empty Object

private

   --  The actual implementation of an NVList:
   --  a list of NamedValues.
   package NV_Sequence is new Sequences.Unbounded (CORBA.NamedValue);

   type Object is new CORBA.Impl.Object with record
      List : NV_Sequence.Sequence := NV_Sequence.Null_Sequence;
   end record;

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Ref with null record);

end CORBA.NVList;
