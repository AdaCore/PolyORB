------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . P O A                             --
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

with Ada.Task_Attributes;
with Broca.Sequences;
with Broca.Exceptions;
with Broca.Object;
with Broca.Refs;
with Broca.IOP;
with Broca.CDR;     use Broca.CDR;
with Broca.Buffers; use Broca.Buffers;
with Broca.Server;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.POA is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.poa");
   procedure O is new Broca.Debug.Output (Flag);

   type POA_Task_Attribute is record
      Current_Object    : PortableServer.ObjectId;
      Current_POA       : PortableServer.POA_Forward.Ref;
   end record;

   type POA_Task_Attribute_Handle is
     access POA_Task_Attribute;

   Nil_Attribute : POA_Task_Attribute_Handle := null;

   package Attributes is new Ada.Task_Attributes
     (Attribute => POA_Task_Attribute_Handle,
      Initial_Value => Nil_Attribute);

   --------------------------
   -- Get_Attributes_Value --
   --------------------------

   procedure Get_Attributes_Value
     (Current_Object    : out PortableServer.ObjectId;
      Current_POA       : out PortableServer.POA_Forward.Ref)
   is
   begin
      pragma Assert (Attributes.Value /= null);
      Current_Object := Attributes.Value.Current_Object;
      Current_POA    := Attributes.Value.Current_POA;
   end Get_Attributes_Value;

   ------------------------
   -- Get_The_POAManager --
   ------------------------

   function Get_The_POAManager
     (Self : access POA_Object)
     return POAManager_Object_Ptr is
   begin
      return Self.POA_Manager;
   end Get_The_POAManager;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : Skeleton) is
   begin
      Broca.CDR.Marshall (Buffer, Skeleton_To_Ref (Value));
   end Marshall;

   -------------------
   -- POA_Object_Of --
   -------------------

   function POA_Object_Of (The_Ref : Ref) return POA_Object_Ptr is
   begin
      return POA_Object_Ptr (Object_Of (The_Ref));
   end POA_Object_Of;

   ---------------------
   -- Ref_To_Skeleton --
   ---------------------

   function Ref_To_Skeleton
     (Ref : CORBA.Object.Ref'Class)
     return Skeleton_Ptr
   is
      use CORBA.Impl;

      Obj : constant Object_Ptr := CORBA.Object.Object_Of (Ref);

   begin
      --  FIXME: Creating uncontrolled reference to
      --    a reference-counted object.
      if Obj = null then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      if Obj.all in Skeleton'Class then
         return Skeleton_Ptr (Obj);
      end if;

      if Obj.all not in Object.Object_Type'Class then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      declare
         use Broca.IOP;
         use Broca.Sequences;
         use Broca.Server;
         use Broca.Sequences.Octet_Sequences;

         X : Object.Object_Ptr := Object.Object_Ptr (Obj);
         P : Profile_Ptr := X.Profiles (X.Profiles'First);
         E : aliased Encapsulation := To_Octet_Array (Get_Object_Key (P.all));

         Key       : aliased Buffer_Type;
         POA       : POA_Object_Ptr;
         POA_Ref   : Broca.POA.Ref;
         POA_State : Processing_State_Type;

      begin
         Decapsulate (E'Access, Key'Access);
         Server.Unmarshall (Key'Access, POA_Ref, POA_State);
         POA := POA_Object_Of (POA_Ref);

         declare
            E : aliased Encapsulation := Unmarshall (Key'Access);

         begin
            return Key_To_Skeleton (POA, E'Unchecked_Access);
         end;
      end;
   end Ref_To_Skeleton;

   ---------
   -- Set --
   ---------

   procedure Set
     (The_Ref : in out Ref;
      The_Object : POA_Object_Ptr) is
   begin
      Broca.Refs.Set
        (Broca.Refs.Ref (The_Ref), Broca.Refs.Ref_Ptr (The_Object));
   end Set;

   --------------------------
   -- Set_Attributes_Value --
   --------------------------

   procedure Set_Attributes_Value
     (Current_Object    : in PortableServer.ObjectId;
      Current_POA       : in PortableServer.POA_Forward.Ref) is
   begin
      if Attributes.Value = Nil_Attribute then
         Attributes.Set_Value (new POA_Task_Attribute);
      end if;

      Attributes.Value.all := POA_Task_Attribute'(Current_Object, Current_POA);
   end Set_Attributes_Value;

   ---------------------
   -- Skeleton_To_Ref --
   ---------------------

   function Skeleton_To_Ref
     (Skel : Skeleton)
     return CORBA.Object.Ref is
   begin
      return Server.Build_IOR (Skel.Type_Id, Skel.POA, Skel.Object_Key.all);
   end Skeleton_To_Ref;

end Broca.POA;
