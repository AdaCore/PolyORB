------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with CORBA; use CORBA;
with Broca.Exceptions;
with Broca.Refs;
with Broca.ORB;
with PortableServer.POA;

package body PortableServer is

   package Address_To_Ref_Ptr_Conversions is
     new System.Address_To_Access_Conversions (Broca.Refs.Ref_Type);
   use Address_To_Ref_Ptr_Conversions;

   ---------------------------------------
   -- Information about a skeleton unit --
   ---------------------------------------

   type Skeleton_Info is record
      Type_Id : CORBA.RepositoryId;
      Is_A : Servant_Class_Predicate;
      Dispatcher : GIOP_Dispatcher;
   end record;

   -----------------------------
   -- A list of Skeleton_Info --
   -----------------------------

   type Skeleton_Cell;
   type Skeleton_List is access Skeleton_Cell;

   type Skeleton_Cell is record
      Info : Skeleton_Info;
      Next : Skeleton_List;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Skeleton_Cell, Skeleton_List);

   Skeleton_Exists : exception;
   Skeleton_Unknown : exception;

   protected Skeletons_Repository is

      procedure Register
        (Id : CORBA.RepositoryId;
         Is_A : Servant_Class_Predicate;
         Dispatcher : GIOP_Dispatcher);
      procedure Unregister
        (Id : CORBA.RepositoryId);

      function Find_Info
        (Obj : Servant)
        return Skeleton_Info;

   private

      All_Skeletons : Skeleton_List := null;

   end Skeletons_Repository;

   protected body Skeletons_Repository is

      procedure Register
        (Id : CORBA.RepositoryId;
         Is_A : Servant_Class_Predicate;
         Dispatcher : GIOP_Dispatcher)
      is
         Cur : Skeleton_List := All_Skeletons;
      begin
         while Cur /= null loop
            if Cur.Info.Type_Id = Id then
               raise Skeleton_Exists;
            end if;
            Cur := Cur.Next;
         end loop;

         All_Skeletons := new Skeleton_Cell'
           (Info => (Type_Id => Id,
                     Is_A => Is_A,
                     Dispatcher => Dispatcher),
            Next => All_Skeletons);
      end Register;

      procedure Unregister
        (Id : CORBA.RepositoryId)
      is
         Cur : Skeleton_List := All_Skeletons;
         Prev : Skeleton_List := null;
      begin
         while Cur /= null loop
            exit when Cur.Info.Type_Id = Id;

            Prev := Cur;
            Cur := Cur.Next;
         end loop;

         if Cur = null then
            raise Skeleton_Unknown;
         end if;

         if Prev /= null then
            Prev.Next := Cur.Next;
         else
            All_Skeletons := Cur.Next;
         end if;

         Free (Cur);
      end Unregister;

      function Find_Info
        (Obj : Servant)
        return Skeleton_Info
      is
         Cur : Skeleton_List := All_Skeletons;
      begin
         while Cur /= null loop
            exit when Cur.Info.Is_A (Obj);
            Cur := Cur.Next;
         end loop;

         if Cur = null then
            raise Skeleton_Unknown;
         end if;

         return Cur.Info;
      end Find_Info;
   end Skeletons_Repository;

   procedure Register_Skeleton
     (Id : CORBA.RepositoryId;
      Is_A : Servant_Class_Predicate;
      Dispatcher : GIOP_Dispatcher) is
   begin
      Skeletons_Repository.Register (Id, Is_A, Dispatcher);
   end Register_Skeleton;

   procedure Unregister_Skeleton
     (Id : CORBA.RepositoryId) is
   begin
      Skeletons_Repository.Unregister (Id);
   end Unregister_Skeleton;

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id
     (Obj : Servant)
     return CORBA.RepositoryId
   is
      Info : Skeleton_Info;
   begin
      Info := Skeletons_Repository.Find_Info (Obj);
      return Info.Type_Id;
   exception
      when Skeleton_Unknown =>
         return CORBA.To_CORBA_String ("IDL:omg.org/CORBA/OBJECT:1.0");
      when others =>
         raise;
   end Get_Type_Id;

   -------------------
   -- GIOP_Dispatch --
   -------------------

   procedure GIOP_Dispatch
     (Obj : Servant;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Request_Buffer : access Broca.Buffers.Buffer_Type;
      Reply_Buffer   : access Broca.Buffers.Buffer_Type)
   is
      Info : Skeleton_Info;
   begin
      Info := Skeletons_Repository.Find_Info (Obj);
      Info.Dispatcher (Obj, Operation, Request_Id,
                       Response_Expected, Request_Buffer,
                       Reply_Buffer);
   exception
      when Skeleton_Unknown =>
         Broca.Exceptions.Raise_Bad_Operation;
      when others =>
         raise;
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
      Excp_Mb : ForwardRequest_Members
        := (Forward_Reference => Reference);
   begin
      Broca.Exceptions.User_Raise_Exception (ForwardRequest'Identity,
                                             Excp_Mb);
   end Raise_Forward_Request;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members)
   is
   begin
      Broca.Exceptions.User_Get_Members (From, To);
   end Get_Members;

end PortableServer;
