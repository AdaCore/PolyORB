------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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
with Ada.Unchecked_Conversion;

with CORBA; use CORBA;

with Broca.Exceptions;
with Broca.Names; use Broca.Names;
with Broca.Sequences;
with Broca.ORB;
with Broca.Soft_Links; use Broca.Soft_Links;
with Broca.Configuration;
pragma Warnings (Off, Broca.Configuration);
pragma Elaborate (Broca.Configuration);

with PortableServer.POA;

package body PortableServer is

   ---------------------------------------
   -- Information about a skeleton unit --
   ---------------------------------------

   type Skeleton_Info is record
      Type_Id    : CORBA.RepositoryId;
      Is_A       : Servant_Class_Predicate;
      Dispatcher : GIOP_Dispatcher;
   end record;

   -----------------------------
   -- A list of Skeleton_Info --
   -----------------------------

   type Skeleton_Node;
   type Skeleton_List is access Skeleton_Node;

   type Skeleton_Node is record
      Info : Skeleton_Info;
      Next : Skeleton_List;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Skeleton_Node, Skeleton_List);

   Skeleton_Exists  : exception;
   Skeleton_Unknown : exception;

   All_Skeletons : Skeleton_List;

   function Find_Info
     (For_Servant : Servant)
     return Skeleton_Info;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Info
     (For_Servant : Servant)
     return Skeleton_Info
   is
      Current : Skeleton_List;
      Info    : Skeleton_Info;

   begin
      Enter_Critical_Section;
      Current := All_Skeletons;
      while Current /= null loop
         exit when Current.Info.Is_A (For_Servant);
         Current := Current.Next;
      end loop;

      if Current = null then
         Leave_Critical_Section;
         raise Skeleton_Unknown;
      end if;

      Info := Current.Info;
      Leave_Critical_Section;

      return Info;
   end Find_Info;

   -----------------------
   -- Register_Skeleton --
   -----------------------

   procedure Register_Skeleton
     (Type_Id    : in CORBA.RepositoryId;
      Is_A       : in Servant_Class_Predicate;
      Dispatcher : in GIOP_Dispatcher)
   is
      Current : Skeleton_List;

   begin
      Enter_Critical_Section;
      Current := All_Skeletons;
      while Current /= null loop
         if Current.Info.Type_Id = Type_Id then
            Leave_Critical_Section;
            raise Skeleton_Exists;
         end if;
         Current := Current.Next;
      end loop;

      All_Skeletons := new Skeleton_Node'
           (Info => (Type_Id    => Type_Id,
                     Is_A       => Is_A,
                     Dispatcher => Dispatcher),
            Next => All_Skeletons);
      Leave_Critical_Section;
   end Register_Skeleton;

   -----------------------
   -- Register_Skeleton --
   -----------------------

   procedure Unregister_Skeleton
     (Type_Id : in CORBA.RepositoryId)
   is
      Current  : Skeleton_List;
      Previous : Skeleton_List;

   begin
      Enter_Critical_Section;
      Current := All_Skeletons;
      while Current /= null loop
         exit when Current.Info.Type_Id = Type_Id;

         Previous := Current;
         Current  := Current.Next;
      end loop;

      if Current = null then
         Leave_Critical_Section;
         raise Skeleton_Unknown;
      end if;

      if Previous /= null then
         Previous.Next := Current.Next;

      else
         All_Skeletons := Current.Next;
      end if;

      Free (Current);
      Leave_Critical_Section;
   end Unregister_Skeleton;

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id
     (For_Servant : Servant)
     return CORBA.RepositoryId is
   begin
      return Find_Info (For_Servant).Type_Id;
   exception
      when Skeleton_Unknown =>
         return CORBA.To_CORBA_String (OMG_RepositoryId ("OBJECT"));
      when others =>
         raise;
   end Get_Type_Id;

   -------------------
   -- GIOP_Dispatch --
   -------------------

   procedure GIOP_Dispatch
     (For_Servant       : in Servant;
      Operation         : in String;
      Request_Id        : in CORBA.Unsigned_Long;
      Response_Expected : in CORBA.Boolean;
      Request_Buffer    : access Broca.Buffers.Buffer_Type;
      Reply_Buffer      : access Broca.Buffers.Buffer_Type)
   is
      Info : Skeleton_Info;

   begin
      Info := Find_Info (For_Servant);
      Info.Dispatcher
        (For_Servant,
         Operation,
         Request_Id,
         Response_Expected,
         Request_Buffer,
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
         (Broca.ORB.Resolve_Initial_References
          (Broca.ORB.Root_POA_ObjectId)));
   end Get_Default_POA;

   ---------------------------
   -- Raise_Forward_Request --
   ---------------------------

   procedure Raise_Forward_Request
     (Reference : in CORBA.Object.Ref)
   is
      Excp_Mb : ForwardRequest_Members := (Forward_Reference => Reference);

   begin
      Broca.Exceptions.User_Raise_Exception
        (ForwardRequest'Identity, Excp_Mb);
   end Raise_Forward_Request;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members) is
   begin
      Broca.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   function ObjectId_To_Octet_Sequence is
      new Ada.Unchecked_Conversion
       (ObjectId, Broca.Sequences.Octet_Sequence);

   function Octet_Sequence_To_ObjectId is
      new Ada.Unchecked_Conversion
       (Broca.Sequences.Octet_Sequence, ObjectId);

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data   : in ObjectId) is
   begin
      Broca.Sequences.Marshall
        (Buffer, ObjectId_To_Octet_Sequence (Data));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type)
     return ObjectId is
   begin
      return Octet_Sequence_To_ObjectId
        (Broca.Sequences.Unmarshall (Buffer));
   end Unmarshall;

end PortableServer;
