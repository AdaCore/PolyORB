------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C O S N O T I F Y F I L T E R . F I L T E R . I M P L           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

package CosNotifyFilter.Filter.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_Constraint_Grammar
     (Self : access Object)
     return CORBA.String;

   function Add_Constraints
     (Self            : access Object;
      Constraint_List : in CosNotifyFilter.ConstraintExpSeq)
     return CosNotifyFilter.ConstraintInfoSeq;

   procedure Modify_Constraints
     (Self        : access Object;
      Del_List    : in CosNotifyFilter.ConstraintIDSeq;
      Modify_List : in CosNotifyFilter.ConstraintInfoSeq);

   function Get_Constraints
     (Self    : access Object;
      Id_List : in CosNotifyFilter.ConstraintIDSeq)
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
      Filterable_Data : in CORBA.Any)
     return CORBA.Boolean;

   function Match_Structured
     (Self            : access Object;
      Filterable_Data : in CosNotification.StructuredEvent)
     return CORBA.Boolean;

   function Match_Typed
     (Self            : access Object;
      Filterable_Data : in CosNotification.PropertySeq)
     return CORBA.Boolean;

   function Attach_Callback
     (Self     : access Object;
      Callback : in CosNotifyComm.NotifySubscribe.Ref)
     return CosNotifyFilter.CallbackID;

   procedure Detach_Callback
     (Self     : access Object;
      Callback : in CosNotifyFilter.CallbackID);

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
