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

with Broca.Exceptions;
with Broca.Refs;
with Broca.Sequences;
with Broca.CDR;
with Broca.Buffers; use Broca.Buffers;
with Broca.Server;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.POA is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.poa");
   procedure O is new Broca.Debug.Output (Flag);

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
      Broca.CDR.Marshall
        (Buffer,
         Skeleton_To_Ref (Value));
   end Marshall;

   -----------------
   -- To_Skeleton --
   -----------------

   --  XXX Rename to Skeleton_Of and make it
   --  consistent with Object_Of.

   function To_Skeleton
     (Ref : CORBA.Object.Ref'Class)
     return Skeleton_Ptr
   is
      use CORBA.Impl;

      Res : constant Object_Ptr
        := CORBA.Object.Object_Of (Ref);

   begin
      --  FIXME: Creating uncontrolled reference to
      --    a reference-counted object.
      if Res = null or else Res.all not in Skeleton'Class then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      return Skeleton_Ptr (Res);
   end To_Skeleton;

   --------------------------
   -- To_Internal_Skeleton --
   --------------------------

   --  XXX Rename to Internal_Skeleton_Of
   --  and make it consistent with Object_Of.

   function To_Internal_Skeleton
     (Ref : CORBA.Object.Ref'Class)
     return Internal_Skeleton_Ptr
   is
      use CORBA.Impl;

      Res : constant Object_Ptr
        := CORBA.Object.Object_Of (Ref);

   begin
      --  FIXME: Creating uncontrolled reference to
      --    a reference-counted object.
      if Res = null or else Res.all not in Internal_Skeleton'Class then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      return Internal_Skeleton_Ptr (Res);
   end To_Internal_Skeleton;

   ---------------------
   -- Skeleton_To_Ref --
   ---------------------

   function Skeleton_To_Ref
     (Skel : Skeleton)
     return CORBA.Object.Ref
   is
      use Broca.Sequences;
      use Broca.Sequences.Octet_Sequences;

      IOR_Encapsulation : aliased Encapsulation
        := Broca.Server.Build_IOR
        (Skel.Type_Id, Skel.POA, Skel.Object_Key.all);

      B : aliased Buffer_Type;
      R : CORBA.Object.Ref;

   begin
      Broca.Buffers.Decapsulate (IOR_Encapsulation'Access, B'Access);
      Show (B);
      Broca.CDR.Unmarshall (B'Access, R);
      Release (B);
      return R;
   end Skeleton_To_Ref;

   ------------------------------
   -- Create_Internal_Skeleton --
   ------------------------------

   function Create_Internal_Skeleton
     (P_Servant : PortableServer.Servant)
     return Internal_Skeleton_Ptr
   is
      Res : Internal_Skeleton_Ptr;

   begin
      Res := new Internal_Skeleton;
      Res.P_Servant := P_Servant;
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Res));
      return Res;
   end Create_Internal_Skeleton;

   -------------------
   -- POA_Object_Of --
   -------------------

   function POA_Object_Of (The_Ref : Ref) return POA_Object_Ptr is
   begin
      return POA_Object_Ptr (Object_Of (The_Ref));
   end POA_Object_Of;

   ---------
   -- Set --
   ---------

   procedure Set
     (The_Ref : in out Ref;
      The_Object : POA_Object_Ptr) is
   begin
      Broca.Refs.Set
        (Broca.Refs.Ref (The_Ref),
         Broca.Refs.Ref_Ptr (The_Object));
   end Set;

end Broca.POA;
