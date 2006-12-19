------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  C O R B A . R E P O S I T O R Y _ R O O T . A R R A Y D E F . I M P L   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;

package CORBA.Repository_Root.ArrayDef.Impl is

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Length : CORBA.Unsigned_Long;
                   Element_Type_Def : CORBA.Repository_Root.IDLType.Ref);

   --  overload the get_type from IDLType
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_length
     (Self : access Object)
     return CORBA.Unsigned_Long;

   procedure set_length
     (Self : access Object;
      To : CORBA.Unsigned_Long);

   function get_element_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_element_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_element_type_def
     (Self : access Object;
      To : CORBA.Repository_Root.IDLType.Ref);

private

   type Object is new CORBA.Repository_Root.IDLType.Impl.Object with record
      Length : CORBA.Unsigned_Long;
      --  the Element_Type field is the one from the IDLType
      Element_Type_Def : CORBA.Repository_Root.IDLType.Ref;
   end record;

end CORBA.Repository_Root.ArrayDef.Impl;
