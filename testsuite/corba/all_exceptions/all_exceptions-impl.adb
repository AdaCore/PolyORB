------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A L L _ E X C E P T I O N S . I M P L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with all_exceptions.Skel;
pragma Warnings (Off, all_exceptions.Skel);

package body all_exceptions.Impl is

   procedure Unknown_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

   begin
      raise Constraint_Error;
   end Unknown_exception_test;

   procedure Bad_Param_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Bad_Param_Members;

   begin
      Member := (Minor => 1, Completed => CORBA.Completed_Maybe);
      CORBA.Raise_Bad_Param (Member);
   end Bad_Param_exception_test;

   procedure No_Memory_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.No_Memory_Members;

   begin
      Member := (Minor => 102, Completed => CORBA.Completed_Yes);
      CORBA.Raise_No_Memory (Member);
   end No_Memory_exception_test;

   procedure Imp_Limit_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Imp_Limit_Members;

   begin
      Member := (Minor => 103, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Imp_Limit (Member);
   end Imp_Limit_exception_test;

   procedure Comm_Failure_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Comm_Failure_Members;

   begin
      Member := (Minor => 104, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Comm_Failure (Member);
   end Comm_Failure_exception_test;

   procedure Inv_Objref_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Inv_Objref_Members;

   begin
      Member := (Minor => 105, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Inv_Objref (Member);
   end Inv_Objref_exception_test;

   procedure No_Permission_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.No_Permission_Members;

   begin
      Member := (Minor => 106, Completed => CORBA.Completed_Yes);
      CORBA.Raise_No_Permission (Member);
   end No_Permission_exception_test;

   procedure Internal_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Internal_Members;

   begin
      Member := (Minor => 107, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Internal (Member);
   end Internal_exception_test;

   procedure Marshal_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Marshal_Members;

   begin
      Member := (Minor => 108, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Marshal (Member);
   end Marshal_exception_test;

   procedure Initialization_Failure_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Initialization_Failure_Members;

   begin
      Member := (Minor => 109, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Initialization_Failure (Member);
   end Initialization_Failure_exception_test;

   procedure No_Implement_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.No_Implement_Members;

   begin
      Member := (Minor => 110, Completed => CORBA.Completed_Yes);
      CORBA.Raise_No_Implement (Member);
   end No_Implement_exception_test;

   procedure Bad_Typecode_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Bad_Typecode_Members;

   begin
      Member := (Minor => 111, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Bad_TypeCode (Member);
   end Bad_Typecode_exception_test;

   procedure Bad_Operation_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Bad_Operation_Members;

   begin
      Member := (Minor => 112, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Bad_Operation (Member);
   end Bad_Operation_exception_test;

   procedure No_Resources_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.No_Resources_Members;

   begin
      Member := (Minor => 113, Completed => CORBA.Completed_Yes);
      CORBA.Raise_No_Resources (Member);
   end No_Resources_exception_test;

   procedure No_Response_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.No_Response_Members;

   begin
      Member := (Minor => 114, Completed => CORBA.Completed_Yes);
      CORBA.Raise_No_Response (Member);
   end No_Response_exception_test;

   procedure Persist_Store_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Persist_Store_Members;

   begin
      Member := (Minor => 115, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Persist_Store (Member);
   end Persist_Store_exception_test;

   procedure Bad_Inv_Order_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Bad_Inv_Order_Members;

   begin
      Member := (Minor => 116, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Bad_Inv_Order (Member);
   end Bad_Inv_Order_exception_test;

   procedure Transient_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Transient_Members;

   begin
      Member := (Minor => 117, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Transient (Member);
   end Transient_exception_test;

   procedure Free_Mem_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Free_Mem_Members;

   begin
      Member := (Minor => 118, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Free_Mem (Member);
   end Free_Mem_exception_test;

   procedure Inv_Ident_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Inv_Ident_Members;

   begin
      Member := (Minor => 119, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Inv_Ident (Member);
   end Inv_Ident_exception_test;

   procedure Inv_Flag_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Inv_Flag_Members;

   begin
      Member := (Minor => 120, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Inv_Flag (Member);
   end Inv_Flag_exception_test;

   procedure Intf_Repos_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Intf_Repos_Members;

   begin
      Member := (Minor => 121, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Intf_Repos (Member);
   end Intf_Repos_exception_test;

   procedure Bad_Context_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Bad_Context_Members;

   begin
      Member := (Minor => 122, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Bad_Context (Member);
   end Bad_Context_exception_test;

   procedure Obj_Adapter_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Obj_Adapter_Members;

   begin
      Member := (Minor => 123, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Obj_Adapter (Member);
   end Obj_Adapter_exception_test;

   procedure Data_Conversion_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Data_Conversion_Members;

   begin
      Member := (Minor => 124, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Data_Conversion (Member);
   end Data_Conversion_exception_test;

   procedure Object_Not_Exist_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Object_Not_Exist_Members;

   begin
      Member := (Minor => 125, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Object_Not_Exist (Member);
   end Object_Not_Exist_exception_test;

   procedure Transaction_Required_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Transaction_Required_Members;

   begin
      Member := (Minor => 126, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Transaction_Required (Member);
   end Transaction_Required_exception_test;

   procedure Transaction_Rolledback_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Transaction_Rolledback_Members;

   begin
      Member := (Minor => 127, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Transaction_Rolledback (Member);
   end Transaction_Rolledback_exception_test;

   procedure Invalid_Transaction_exception_test
     (Self : access Object)
   is
      pragma Unreferenced (Self);

      Member : CORBA.Invalid_Transaction_Members;

   begin
      Member := (Minor => 128, Completed => CORBA.Completed_Yes);
      CORBA.Raise_Invalid_Transaction (Member);
   end Invalid_Transaction_exception_test;

end all_exceptions.Impl;
