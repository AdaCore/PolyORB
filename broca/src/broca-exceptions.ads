------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     B R O C A . E X C E P T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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

with System;
with Ada.Exceptions;
with CORBA; use CORBA;
with Broca.Buffers; use Broca.Buffers;

package Broca.Exceptions is
   -----------------------------------------
   --  Declarations for user exceptions.  --
   -----------------------------------------

   type IDL_Exception_Members_Ptr is
      access all CORBA.IDL_Exception_Members'Class;

   --  Extract members from an exception occurence.
   procedure User_Get_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence;
      Members : out CORBA.IDL_Exception_Members'Class);

   --  Raise an user exception.
   procedure User_Raise_Exception
     (Id : Ada.Exceptions.Exception_Id; Members : IDL_Exception_Members_Ptr);

   -------------------------------------------
   --  Declarations for system exceptions.  --
   -------------------------------------------

   --  Raise CORBA.bad_param with minor = 0.
   procedure Raise_Bad_Param (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Param);

   --  Raise CORBA.marshal with minor = 0.
   procedure Raise_Marshal (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Marshal);

   --  Raise CORBA.comm_failure with minor = 0.
   procedure Raise_Comm_Failure (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Comm_Failure);

   --  Raise CORBA.inv_objref with minor = 0.
   procedure Raise_Inv_Objref (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Inv_Objref);

   --  Raise CORBA.object_not_exist with minor = 0.
   procedure Raise_Object_Not_Exist
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Object_Not_Exist);

   --  Raise CORBA.obj_adapter with minor = 0.
   procedure Raise_Obj_Adapter
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Obj_Adapter);

   --  Raise CORBA.bad_operation with minor = 0.
   procedure Raise_Bad_Operation
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Operation);

   --  Raise CORBA.transient with minor = 0.
   procedure Raise_Transient
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Transient);

   --  Raise CORBA.no_implement with minor = 0.
   procedure Raise_No_Implement
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_No_Implement);

   --  Raise CORBA.internal
   procedure Raise_Internal
     (Minor : Unsigned_Long; Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Internal);

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);

   --  Only for a system exception.
   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Excpt  : in CORBA.Exception_Occurrence);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Excpt  : in CORBA.Exception_Occurrence);

   procedure Unmarshall_And_Raise (Buffer : in out Buffer_Descriptor);
   pragma No_Return (Unmarshall_And_Raise);

   procedure Raise_With_Address (Id : Ada.Exceptions.Exception_Id;
                                 Addr : System.Address);
   pragma No_Return (Raise_With_Address);

   procedure Get_Member (Occurrence : Ada.Exceptions.Exception_Occurrence;
                         Addr : out System.Address);
end Broca.Exceptions;
