------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A L L _ E X C E P T I O N S . I M P L                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with PortableServer;

package all_exceptions.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object;

   procedure Unknown_exception_test
     (Self : access Object);

   procedure Bad_Param_exception_test
     (Self : access Object);

   procedure No_Memory_exception_test
     (Self : access Object);

   procedure Imp_Limit_exception_test
     (Self : access Object);

   procedure Comm_Failure_exception_test
     (Self : access Object);

   procedure Inv_Objref_exception_test
     (Self : access Object);

   procedure No_Permission_exception_test
     (Self : access Object);

   procedure Internal_exception_test
     (Self : access Object);

   procedure Marshal_exception_test
     (Self : access Object);

   procedure Initialization_Failure_exception_test
     (Self : access Object);

   procedure No_Implement_exception_test
     (Self : access Object);

   procedure Bad_Typecode_exception_test
     (Self : access Object);

   procedure Bad_Operation_exception_test
     (Self : access Object);

   procedure No_Resources_exception_test
     (Self : access Object);

   procedure No_Response_exception_test
     (Self : access Object);

   procedure Persist_Store_exception_test
     (Self : access Object);

   procedure Bad_Inv_Order_exception_test
     (Self : access Object);

   procedure Transient_exception_test
     (Self : access Object);

   procedure Free_Mem_exception_test
     (Self : access Object);

   procedure Inv_Ident_exception_test
     (Self : access Object);

   procedure Inv_Flag_exception_test
     (Self : access Object);

   procedure Intf_Repos_exception_test
     (Self : access Object);

   procedure Bad_Context_exception_test
     (Self : access Object);

   procedure Obj_Adapter_exception_test
     (Self : access Object);

   procedure Data_Conversion_exception_test
     (Self : access Object);

   procedure Object_Not_Exist_exception_test
     (Self : access Object);

   procedure Transaction_Required_exception_test
     (Self : access Object);

   procedure Transaction_Rolledback_exception_test
     (Self : access Object);

   procedure Invalid_Transaction_exception_test
     (Self : access Object);

private

   type Object is new PortableServer.Servant_Base with null record;

end all_exceptions.Impl;
