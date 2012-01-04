------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A L L _ E X C E P T I O N S . I M P L                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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
