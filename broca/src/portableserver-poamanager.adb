------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A M A N A G E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
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

with Broca.Exceptions;
with Broca.POA; use Broca.POA;
with Broca.Refs;

package body PortableServer.POAManager is
   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                          To   : out AdapterInactive_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= AdapterInactive'Identity then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      To := AdapterInactive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   --  Convert a REF to a POAManager_Object_Ptr.
   --  Check the type of the referenced object.
   function To_Poa_Manager (Self : Ref) return POAManager_Object_Ptr;

   function To_Poa_Manager (Self : Ref)
                            return POAManager_Object_Ptr
   is
      use Broca.Refs;
      Res : Broca.Refs.Ref_Ptr;
   begin
      Res := Get (Self);
      if Res = null or else Res.all not in POAManager_Object'Class then
         Broca.Exceptions.Raise_Bad_Param;
      else
         return POAManager_Object_Ptr (Res);
      end if;
   end To_Poa_Manager;

   procedure Activate (Self : Ref) is
      Poa_Manager : POAManager_Object_Ptr;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Activate (Poa_Manager.all);
      end if;
   end Activate;

   procedure Hold_Requests (Self : Ref; Wait_For_Completion : CORBA.Boolean)
   is
      Poa_Manager : POAManager_Object_Ptr;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Hold_Requests (Poa_Manager.all, Wait_For_Completion);
      end if;
   end Hold_Requests;

   procedure Discard_Requests (Self : Ref; Wait_For_Completion : CORBA.Boolean)
   is
      Poa_Manager : POAManager_Object_Ptr;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Discard_Requests (Poa_Manager.all, Wait_For_Completion);
      end if;
   end Discard_Requests;

   procedure Deactivate
     (Self : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      Poa_Manager : POAManager_Object_Ptr;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Deactivate
           (Poa_Manager.all, Etherealize_Objects, Wait_For_Completion);
      end if;
   end Deactivate;

end PortableServer.POAManager;
