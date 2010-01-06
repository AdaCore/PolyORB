------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    COSNOTIFYFILTER.MAPPINGFILTER.IMPL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  with CORBA;

with PortableServer;

package CosNotifyFilter.MappingFilter.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_Constraint_Grammar
     (Self : access Object)
     return CORBA.String;

   function Get_Value_Type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function Get_Default_Value
     (Self : access Object)
     return CORBA.Any;

   function Add_Mapping_Constraints
     (Self      : access Object;
      Pair_List : CosNotifyFilter.MappingConstraintPairSeq)
     return CosNotifyFilter.MappingConstraintInfoSeq;

   procedure Modify_Mapping_Constraints
     (Self        : access Object;
      Del_List    : CosNotifyFilter.ConstraintIDSeq;
      Modify_List : CosNotifyFilter.MappingConstraintInfoSeq);

   function Get_Mapping_Constraints
     (Self    : access Object;
      Id_List : CosNotifyFilter.ConstraintIDSeq)
     return CosNotifyFilter.MappingConstraintInfoSeq;

   function Get_All_Mapping_Constraints
     (Self : access Object)
     return CosNotifyFilter.MappingConstraintInfoSeq;

   procedure Remove_All_Mapping_Constraints
     (Self : access Object);

   procedure Destroy
     (Self : access Object);

   procedure Match
     (Self            : access Object;
      Filterable_Data : CORBA.Any;
      Result_To_Set   : out CORBA.Any;
      Returns         : out CORBA.Boolean);

   procedure Match_Structured
     (Self            : access Object;
      Filterable_Data : CosNotification.StructuredEvent;
      Result_To_Set   : out CORBA.Any;
      Returns         : out CORBA.Boolean);

   procedure Match_Typed
     (Self            : access Object;
      Filterable_Data : CosNotification.PropertySeq;
      Result_To_Set   : out CORBA.Any;
      Returns         : out CORBA.Boolean);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

private

   type Mapping_Filter_Record;
   type Mapping_Filter_Access is access Mapping_Filter_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Mapping_Filter_Access;
   end record;

end CosNotifyFilter.MappingFilter.Impl;
