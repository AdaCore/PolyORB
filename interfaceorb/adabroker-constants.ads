------------------------------------------------------------------------------
--                                                                          --
--                        ADABROKER COMPONENTS                              --
--                                                                          --
--                  A D A B R O K E R . C O N S T A N T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the repository ids of CORBA exceptions.

package AdaBroker.Constants is

   type Exception_Id is new Standard.String;

   function To_Standard_String
     (S : in Exception_Id)
      return Standard.String;
   --  Transforms a corba string into the corresponding standard string

   function To_Exception_Id
     (S : in Standard.String)
      return Exception_Id;
   --  Transforms a standard string into the corresponding corba string


   Unknown_Repoid   : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/UNKNOWN:1.0");

   Bad_Param_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/BAD_PARAM:1.0");

   No_Memory_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/NO_MEMORY:1.0");

   Imp_Limit_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/IMP_LIMIT:1.0");

   Comm_Failure_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/COMM_FAILURE:1.0");

   Inv_ObjRef_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INV_OBJREF:1.0");

   No_Permission_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/NO_PERMISSION:1.0");

   Internal_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INTERNAL:1.0");

   Marshal_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/MARSHAL:1.0");

   Initialization_Failure_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INITIALIZE:1.0");

   No_Implement_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/NO_IMPLEMENT:1.0");

   Bad_Typecode_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/BAD_TYPECODE:1.0");

   Bad_Operation_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/BAD_OPERATION:1.0");

   No_Resources_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/NO_RESSOURCES:1.0");

   No_Response_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/NO_RESPONSE:1.0");

   Persist_Store_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/PERSIST_STORE:1.0");

   Bad_Inv_Order_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/BAD_INV_ORDER:1.0");

   Transient_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/TRANSIENT:1.0");

   Free_Mem_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/FREE_MEM:1.0");

   Inv_Ident_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INV_IDENT:1.0");

   Inv_Flag_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INV_FLAG:1.0");

   Intf_Repos_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INTF_REPOS:1.0");

   Bad_Context_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/BAD_CONTEXT:1.0");

   Obj_Adapter_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/OBJ_ADAPTER:1.0");

   Data_Conversion_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/DATA_CONVERSION:1.0");

   Object_Not_Exist_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0");

   Transaction_Required_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/TRANSACTION_REQUIRED:1.0");

   Transaction_Rolledback_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/TRANSACTION_ROLLEDBACK:1.0");

   Invalid_Transaction_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/INVALID_TRANSACTION:1.0");

   Wrong_Transaction_Repoid : constant Exception_Id
     := Exception_Id'("IDL:omg.org/CORBA/WRONG_TRANSACTION:1.0");

end AdaBroker.Constants;
