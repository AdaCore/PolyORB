------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     COSNOTIFYFILTER.FILTERFACTORY.IMPL                   --
--                                                                          --
--                                 B o d y                                  --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosNotifyFilter.FilterFactory.Helper;
pragma Elaborate (CosNotifyFilter.FilterFactory.Helper);
pragma Warnings (Off, CosNotifyFilter.FilterFactory.Helper);

--  with CosNotifyFilter.FilterFactory.Skel;
--  pragma Elaborate (CosNotifyFilter.FilterFactory.Skel);
--  pragma Warnings (Off, CosNotifyFilter.FilterFactory.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
--  with PolyORB.Tasking.Semaphores;
with PolyORB.Log;

package body CosNotifyFilter.FilterFactory.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   --  use PolyORB.Tasking.Semaphores;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("filterfactory");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Filter_Factory_Record is record
      This    : Object_Ptr;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   T_Initialized : Boolean := False;
   Self_Mutex : Mutex_Access;

   procedure Ensure_Initialization is
   begin
      if not T_Initialized then
         Create (Self_Mutex);
         T_Initialized := True;
      end if;
   end Ensure_Initialization;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Self               : access Object;
      Constraint_Grammar : in CORBA.String)
     return CosNotifyFilter.Filter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Constraint_Grammar);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.Filter.Ref;
   begin
      pragma Debug (O ("create_filter in filterfactory"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Create_Filter;

   ---------------------------
   -- Create_Mapping_Filter --
   ---------------------------

   function Create_Mapping_Filter
     (Self               : access Object;
      Constraint_Grammar : in CORBA.String;
      Default_Value      : in CORBA.Any)
     return CosNotifyFilter.MappingFilter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Constraint_Grammar, Default_Value);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.MappingFilter.Ref;
   begin
      pragma Debug (O ("create_mapping_filter in filterfactory"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Create_Mapping_Filter;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Factory : Object_Ptr;
      My_Ref  : CosNotifyFilter.FilterFactory.Ref;
   begin
      pragma Debug (O ("create filterfactory"));

      Factory         := new Object;
      Factory.X       := new Filter_Factory_Record;
      Factory.X.This  := Factory;
      Initiate_Servant (Servant (Factory), My_Ref);

      return Factory;
   end Create;

end CosNotifyFilter.FilterFactory.Impl;
