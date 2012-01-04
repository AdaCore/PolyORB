------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   C O S N O T I F Y F I L T E R . M A P P I N G F I L T E R . I M P L    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
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
