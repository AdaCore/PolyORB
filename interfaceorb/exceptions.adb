-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                   package Exceptions                          ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----     This package deals with the raising of C exceptions in    ----
----   Ada and ada ones in C.                                      ----
----     It is both a C and a Ada class (see Ada_Exceptions.hh)    ----
----   and provides 2 mains methods : raise_C_Exception and        ----
----   raise_Ada_Exception. The first one is called by Ada code    ----
----   and implemented in C. The second is called by C code and    ----
----   implemented in Ada. Both translate exceptions in the other  ----
----   language.                                                   ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------

package body Exceptions is


   --------------------------------------------------
   ---       conversion functions                 ---
   --------------------------------------------------

   -- Int_To_Status
   ----------------
   function Int_To_Status (N : in Interfaces.C.Int)
                           return Corba.Completion_Status is
      Ada_N : Integer ;
   begin
      Ada_N := Integer (N) ;
      return Corba.Completion_Status'Val(Ada_N) ;
   end ;


   -- Status_To_Int
   ----------------
   function Status_To_Int (Status : in Corba.Completion_Status)
                           return Interfaces.C.Int is
      Ada_Result : Integer ;
   begin
      Ada_Result := Corba.Completion_Status'Pos(Status) ;
      return Interfaces.C.Int (Ada_Result) ;
   end ;


   ---------------------------------
   -- Handling of Fatal exception --
   ---------------------------------

   -- C_Raise_Ada_Fatal_Exception
   ------------------------------
   procedure C_Raise_Ada_Fatal_Exception (file : in Interfaces.C.Strings.Chars_Ptr ;
                                          Line : in Interfaces.C.Int ;
                                          Err_msg : in Interfaces.C.Strings.Chars_Ptr) is
   begin
      Ada.Exceptions.Raise_Exception (Corba.OmniORB_Fatal_Error'Identity,
                                      "In " &
                                      Interfaces.C.Strings.Value (File) &
                                      ", line " &
                                      Interfaces.C.Int'Image (Line) &
                                      " : " & Corba.CRLF &
                                      Interfaces.C.Strings.Value (Err_Msg)) ;
   end ;



   -----------------------------------
   -- Handling of UNKNOWN exception --
   -----------------------------------

   -- C_Raise_Ada_UNKNOWN_Exception
   --------------------------------
   procedure C_Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Unknown'Identity,
                                   Corba.Unknown_Members'(Minor => Ada_Pd_Minor ,
                                                          Completed => Ada_Pd_Status)) ;
   end ;



   -------------------------------------------------------
   -- And now the same methods for the other exceptions --
   -------------------------------------------------------

   procedure C_Raise_Ada_BAD_PARAM_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Bad_Param'Identity,
                                   Corba.Bad_Param_Members'(Minor => Ada_Pd_Minor ,
                                                            Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_NO_MEMORY_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.No_Memory'Identity,
                                   Corba.No_Memory_Members'(Minor => Ada_Pd_Minor ,
                                                            Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_IMP_LIMIT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Imp_Limit'Identity,
                                   Corba.Imp_Limit_Members'(Minor => Ada_Pd_Minor ,
                                                            Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_COMM_FAILURE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Comm_Failure'Identity,
                                   Corba.Comm_Failure_Members'(Minor => Ada_Pd_Minor ,
                                                               Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INV_OBJREF_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                               Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Inv_Objref'Identity,
                                   Corba.Inv_Objref_Members'(Minor => Ada_Pd_Minor ,
                                                             Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                     Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Object_Not_Exist'Identity,
                                   Corba.Object_Not_Exist_Members'(Minor => Ada_Pd_Minor ,
                                                                   Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_NO_PERMISSION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.No_Permission'Identity,
                                   Corba.No_Permission_Members'(Minor => Ada_Pd_Minor ,
                                                                Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INTERNAL_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                             Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Internal'Identity,
                                   Corba.Internal_Members'(Minor => Ada_Pd_Minor ,
                                                           Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_MARSHAL_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Marshal'Identity,
                                   Corba.Marshal_Members'(Minor => Ada_Pd_Minor ,
                                                          Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                           Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Initialization_Failure'Identity,
                                   Corba.Initialization_Failure_Members'(Minor => Ada_Pd_Minor ,
                                                                         Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.No_Implement'Identity,
                                   Corba.No_Implement_Members'(Minor => Ada_Pd_Minor ,
                                                               Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_BAD_TYPECODE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Bad_Typecode'Identity,
                                   Corba.Bad_Typecode_Members'(Minor => Ada_Pd_Minor ,
                                                               Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_BAD_OPERATION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Bad_Operation'Identity,
                                   Corba.Bad_Operation_Members'(Minor => Ada_Pd_Minor ,
                                                                Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_NO_RESOURCES_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.No_Resources'Identity,
                                   Corba.No_Resources_Members'(Minor => Ada_Pd_Minor ,
                                                               Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_NO_RESPONSE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.No_Response'Identity,
                                   Corba.No_Response_Members'(Minor => Ada_Pd_Minor ,
                                                              Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_PERSIST_STORE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Persist_store'Identity,
                                   Corba.Persist_store_Members'(Minor => Ada_Pd_Minor ,
                                                                Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Bad_Inv_Order'Identity,
                                   Corba.Bad_Inv_Order_Members'(Minor => Ada_Pd_Minor ,
                                                                Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_TRANSIENT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Transient'Identity,
                                   Corba.Transient_Members'(Minor => Ada_Pd_Minor ,
                                                            Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_FREE_MEM_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                             Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Free_Mem'Identity,
                                   Corba.Free_Mem_Members'(Minor => Ada_Pd_Minor ,
                                                           Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INV_IDENT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Inv_Ident'Identity,
                                   Corba.Inv_Ident_Members'(Minor => Ada_Pd_Minor ,
                                                            Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INV_FLAG_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                             Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Inv_Flag'Identity,
                                   Corba.Inv_Flag_Members'(Minor => Ada_Pd_Minor ,
                                                           Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INTF_REPOS_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                               Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Intf_Repos'Identity,
                                   Corba.Intf_Repos_Members'(Minor => Ada_Pd_Minor ,
                                                             Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_BAD_CONTEXT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Bad_Context'Identity,
                                   Corba.Bad_Context_Members'(Minor => Ada_Pd_Minor ,
                                                              Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Obj_Adapter'Identity,
                                   Corba.Obj_Adapter_Members'(Minor => Ada_Pd_Minor ,
                                                              Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_DATA_CONVERSION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                    Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Data_Conversion'Identity,
                                   Corba.Data_Conversion_Members'(Minor => Ada_Pd_Minor ,
                                                                  Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                         Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Transaction_Required'Identity,
                                   Corba.Transaction_Required_Members'(Minor => Ada_Pd_Minor ,
                                                                       Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                           Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Transaction_Rolledback'Identity,
                                   Corba.Transaction_Rolledback_Members'(Minor => Ada_Pd_Minor ,
                                                                         Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                        Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Invalid_Transaction'Identity,
                                   Corba.Invalid_Transaction_Members'(Minor => Ada_Pd_Minor ,
                                                                      Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                      Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.Wrong_Transaction'Identity,
                                   Corba.Wrong_Transaction_Members'(Minor => Ada_Pd_Minor ,
                                                                    Completed => Ada_Pd_Status)) ;
   end ;

   procedure C_Raise_Ada_Fatal_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                          Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and raises the exception
      Corba.Raise_Corba_Exception (Corba.AdaBroker_Fatal_Error'Identity,
                                   Corba.Wrong_Transaction_Members'(Minor => Ada_Pd_Minor ,
                                                                    Completed => Ada_Pd_Status)) ;
   end ;

end Exceptions ;
