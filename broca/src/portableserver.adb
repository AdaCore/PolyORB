------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 B o d y                                  --
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

with System.Address_To_Access_Conversions;
with Ada.Exceptions;
with CORBA;
with Broca.Exceptions;
with Broca.Refs;
with Broca.ORB;
with PortableServer.POA;

package body PortableServer is

   package Address_To_Ref_Ptr_Conversions is
     new System.Address_To_Access_Conversions (Broca.Refs.Ref_Type);
   use Address_To_Ref_Ptr_Conversions;

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id (Obj : Servant_Base) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:omg.org/CORBA/OBJECT:1.0");
   end Get_Type_Id;

   -------------------
   -- GIOP_Dispatch --
   -------------------

   procedure GIOP_Dispatch
     (Obj : access Servant_Base;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Stream : in out Broca.Buffers.Buffer_Descriptor) is
   begin
      Broca.Exceptions.Raise_Bad_Operation;
   end GIOP_Dispatch;

   ---------------------
   -- Get_Default_POA --
   ---------------------

   function Get_Default_POA
     (For_Servant : Servant_Base)
     return POA_Forward.Ref is
   begin
      return PortableServer.POA.Convert.To_Forward
        (POA.To_Ref
         (Broca.ORB.Resolve_Initial_References (Broca.ORB.Root_POA_ObjectId)));
   end Get_Default_POA;

   ---------------------------
   -- Raise_Forward_Request --
   ---------------------------

   procedure Raise_Forward_Request (Reference : CORBA.Object.Ref) is
   begin
      Broca.Exceptions.Raise_With_Address
        (ForwardRequest'Identity,
         To_Address (Object_Pointer (CORBA.Object.Get (Reference))));
   end Raise_Forward_Request;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members)
   is
      use Ada.Exceptions;
      Addr : System.Address;
      Res : CORBA.Object.Ref;
   begin
      if Exception_Identity (From) /= ForwardRequest'Identity then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      Broca.Exceptions.Get_Member (From, Addr);
      CORBA.Object.Set (Res, Broca.Refs.Ref_Ptr (To_Pointer (Addr)));
      To := ForwardRequest_Members'(CORBA.IDL_Exception_Members with Res);
   end Get_Members;

end PortableServer;
