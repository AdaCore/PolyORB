------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . O R B                             --
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

with CORBA.Sequences.Unbounded;
with CORBA.ORB; use CORBA.ORB;
with Broca.IOP;
with CORBA.Object;
with Broca.Exceptions;
with Broca.CDR;
with Broca.Buffers;
with Broca.Refs;
with Broca.Object;
with Broca.Repository;
with Broca.IIOP;
with Broca.IOP;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.ORB is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.orb");
   procedure O is new Broca.Debug.Output (Flag);

   The_ORB : ORB_Ptr := null;

   package IDL_SEQUENCE_Ref is
     new CORBA.Sequences.Unbounded (CORBA.Object.Ref);

   Identifiers : ObjectIdList;
   References  : IDL_SEQUENCE_Ref.Sequence;

   -------------------
   -- IOR_To_Object --
   -------------------

   procedure IOR_To_Object
     (IOR : access Broca.Buffers.Buffer_Type;
      Ref : out CORBA.Object.Ref'Class)
   is
      use Broca.CDR;

      Nbr_Profiles : CORBA.Unsigned_Long;
      Tag : Broca.IOP.Profile_Tag;
      Type_Id : CORBA.String;
      Obj : Broca.Object.Object_Ptr;
   begin
      pragma Debug (O ("IOR_To_Object : enter"));

      --  Unmarshall type id.
      Type_Id := Unmarshall (IOR);
      declare
         A_Ref : CORBA.Object.Ref'Class :=
           Broca.Repository.Create (CORBA.RepositoryId (Type_Id));
      begin
         if CORBA.Object.Is_Nil (A_Ref) then
            --  No classes for the string was found.
            --  Create a CORBA.Object.Ref.
            pragma Debug (O ("IOR_To_Object : A_Ref is nil."));
            Broca.Refs.Set (Broca.Refs.Ref (A_Ref),
                            new Broca.Object.Object_Type);
         end if;

         --  Get the access to the internal object.
         Obj := Broca.Object.Object_Ptr
           (Broca.Refs.Get (Broca.Refs.Ref (A_Ref)));

         Nbr_Profiles := Unmarshall (IOR);

         Obj.Type_Id  := Type_Id;
         Obj.Profiles := new IOP.Profile_Ptr_Array (1 .. Nbr_Profiles);
         for I in 1 .. Nbr_Profiles loop
            Tag := Unmarshall (IOR);
            case Tag is
               when Broca.IOP.Tag_Internet_IOP =>
                  Broca.IIOP.Create_Profile (IOR, Obj.Profiles (I));
               when others =>
                  Broca.Exceptions.Raise_Bad_Param;
            end case;
         end loop;

         --  FIXME: type must be checked ?
         if True then
            --  No.
            Broca.Refs.Set (Broca.Refs.Ref (Ref),
                            Broca.Refs.Get (Broca.Refs.Ref (A_Ref)));
         else
            Ref := A_Ref;
         end if;
      end;
   end IOR_To_Object;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services return ObjectIdList is
   begin
      return Identifiers;
   end List_Initial_Services;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref is
      use CORBA.ORB.IDL_SEQUENCE_ObjectId;
      use IDL_SEQUENCE_Ref;
   begin
      for I in 1 .. Length (Identifiers) loop
         if Element_Of (Identifiers, I) = Identifier then
            return Element_Of (References, I);
         end if;
      end loop;
      raise CORBA.InvalidName;
   end Resolve_Initial_References;

   --------------------------------
   -- Register_Initial_Reference --
   --------------------------------

   procedure Register_Initial_Reference
     (Identifier : in CORBA.ORB.ObjectId;
      Reference  : in CORBA.Object.Ref)
   is
      use CORBA.ORB.IDL_SEQUENCE_ObjectId;
      use IDL_SEQUENCE_Ref;
   begin
      Append (Identifiers, Identifier);
      Append (References, Reference);
   end Register_Initial_Reference;

   ------------------
   -- Register_ORB --
   ------------------

   procedure Register_ORB (ORB : ORB_Ptr) is
   begin
      if The_ORB /= null then
         --  Only one ORB can register.
         Broca.Exceptions.Raise_Internal (1000, CORBA.Completed_No);
      end if;
      The_ORB := ORB;
   end Register_ORB;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      if The_ORB = null then
         return;
      else
         Run (The_ORB.all);
      end if;
   end Run;

   -----------------------
   -- POA_State_Changed --
   -----------------------

   procedure POA_State_Changed
     (POA : in Broca.POA.POA_Object_Ptr) is
   begin
      POA_State_Changed (The_ORB.all, POA);
   end POA_State_Changed;

end Broca.ORB;


