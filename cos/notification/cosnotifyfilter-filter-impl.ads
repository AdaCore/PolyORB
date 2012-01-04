------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C O S N O T I F Y F I L T E R . F I L T E R . I M P L           --
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

package CosNotifyFilter.Filter.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_Constraint_Grammar
     (Self : access Object)
     return CORBA.String;

   function Add_Constraints
     (Self            : access Object;
      Constraint_List : CosNotifyFilter.ConstraintExpSeq)
     return CosNotifyFilter.ConstraintInfoSeq;

   procedure Modify_Constraints
     (Self        : access Object;
      Del_List    : CosNotifyFilter.ConstraintIDSeq;
      Modify_List : CosNotifyFilter.ConstraintInfoSeq);

   function Get_Constraints
     (Self    : access Object;
      Id_List : CosNotifyFilter.ConstraintIDSeq)
     return CosNotifyFilter.ConstraintInfoSeq;

   function Get_All_Constraints
     (Self : access Object)
     return CosNotifyFilter.ConstraintInfoSeq;

   procedure Remove_All_Constraints
     (Self : access Object);

   procedure Destroy
     (Self : access Object);

   function Match
     (Self            : access Object;
      Filterable_Data : CORBA.Any)
     return CORBA.Boolean;

   function Match_Structured
     (Self            : access Object;
      Filterable_Data : CosNotification.StructuredEvent)
     return CORBA.Boolean;

   function Match_Typed
     (Self            : access Object;
      Filterable_Data : CosNotification.PropertySeq)
     return CORBA.Boolean;

   function Attach_Callback
     (Self     : access Object;
      Callback : CosNotifyComm.NotifySubscribe.Ref)
     return CosNotifyFilter.CallbackID;

   procedure Detach_Callback
     (Self     : access Object;
      Callback : CosNotifyFilter.CallbackID);

   function Get_Callbacks
     (Self : access Object)
     return CosNotifyFilter.CallbackIDSeq;

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

private

   type Filter_Record;
   type Filter_Access is access Filter_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Filter_Access;
   end record;

end CosNotifyFilter.Filter.Impl;
