------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.REPOSITORY.IMPL                   --
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

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Container.Impl;

package CORBA.Repository_Root.Repository.Impl is

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Repository.Impl.Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return Repository_Forward.Ref;

   function lookup_id
     (Self : access Object;
      search_id : CORBA.RepositoryId)
     return CORBA.Repository_Root.Contained.Ref;

   function get_canonical_typecode
     (Self : access Object;
      tc : CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function get_primitive
     (Self : access Object;
      kind : CORBA.Repository_Root.PrimitiveKind)
     return CORBA.Repository_Root.PrimitiveDef_Forward.Ref;

   function create_string
     (Self : access Object;
      bound : CORBA.Unsigned_Long)
     return CORBA.Repository_Root.StringDef_Forward.Ref;

   function create_wstring
     (Self : access Object;
      bound : CORBA.Unsigned_Long)
     return CORBA.Repository_Root.WstringDef_Forward.Ref;

   function create_sequence
     (Self : access Object;
      bound : CORBA.Unsigned_Long;
      element_type : CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef_Forward.Ref;

   function create_array
     (Self : access Object;
      length : CORBA.Unsigned_Long;
      element_type : CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.ArrayDef_Forward.Ref;

   function create_fixed
     (Self : access Object;
      IDL_digits : CORBA.Unsigned_Short;
      scale : CORBA.Short)
     return CORBA.Repository_Root.FixedDef_Forward.Ref;

private

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with null record;

end CORBA.Repository_Root.Repository.Impl;
