with CORBA.Sequences.Unbounded;
with CORBA.ORB; use CORBA.ORB;
with CORBA.Iop;
with CORBA.Object;
with Broca.Exceptions;
with Broca.Marshalling;
with Broca.Refs;
with Broca.Object;
with Broca.Repository;
with Broca.Iiop;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Orb is
   Flag : constant Natural := Broca.Debug.Is_Active ("broca.orb");
   procedure O is new Broca.Debug.Output (Flag);

   procedure IOR_To_Object (Ior : in out Broca.Buffers.Buffer_Descriptor;
                            Res : out CORBA.Object.Ref'Class)
   is
      use Broca.Marshalling;

      Nbr_Profiles : CORBA.Unsigned_Long;
      Tag : CORBA.Iop.Profile_Id;
      Type_Id : CORBA.String;
      Obj : Broca.Object.Object_Acc;
   begin
      pragma Debug (O ("Ior_To_Object : enter"));
      --  The IOR is an encapsulation (defined in 13.3.3) as described in
      --  11.6.6
      --  Extract the endianness
      Unmarshall (Ior, Ior.Little_Endian);

      --  Unmarshall type id.
      Unmarshall (Ior, Type_Id);
      declare
         A_Ref : CORBA.Object.Ref'Class :=
           Broca.Repository.Create_Ref (CORBA.RepositoryId (Type_Id));
      begin
         if CORBA.Object.Is_Nil (A_Ref) then
            --  FIXME:
            --  No classes for the string was found.
            --  What can be done ?
	    pragma Debug (O ("Ior_To_Object : A_Ref is nil"));
            Broca.Refs.Set (Broca.Refs.Ref (Res), null);
            return;
         end if;

         --  Get the access to the internal object.
         Obj :=
           Broca.Object.Object_Acc (Broca.Refs.Get (Broca.Refs.Ref (A_Ref)));

         Unmarshall (Ior, Nbr_Profiles);

         Obj.Profiles :=
          new Broca.Object.Profile_Acc_Array (1 .. Nbr_Profiles);
         for I in 1 .. Nbr_Profiles loop
            Unmarshall (Ior, Tag);
            case Tag is
               when CORBA.Iop.Tag_Internet_Iop =>
                  Broca.Iiop.Create_Profile (Ior, Obj.Profiles (I));
               when others =>
                  Broca.Exceptions.Raise_Bad_Param;
            end case;
         end loop;

         --  FIXME: type must be checked ?
         if True then
            --  No.
            Broca.Refs.Set (Broca.Refs.Ref (Res),
                            Broca.Refs.Get (Broca.Refs.Ref (A_Ref)));
         else
            Res := A_Ref;
         end if;
      end;
   end IOR_To_Object;

   package IDL_SEQUENCE_Ref is
     new CORBA.Sequences.Unbounded (CORBA.Object.Ref);
   Identifiers : ObjectIdList;
   References : IDL_SEQUENCE_Ref.Sequence;

   function List_Initial_Services return ObjectIdList is
   begin
      return Identifiers;
   end List_Initial_Services;

   function Resolve_Initial_References (Identifier : ObjectId)
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

   procedure Register_Initial_Reference
     (Identifier : CORBA.ORB.ObjectId; Ref : CORBA.Object.Ref) is
      use CORBA.ORB.IDL_SEQUENCE_ObjectId;
      use IDL_SEQUENCE_Ref;
   begin
      Append (Identifiers, Identifier);
      Append (References, Ref);
   end Register_Initial_Reference;

   The_Orb : Orb_Access := null;

   procedure Register_Orb (Orb : Orb_Access) is
   begin
      if The_Orb /= null then
         --  Only one ORB can register.
         Broca.Exceptions.Raise_Internal (1000, CORBA.Completed_No);
      end if;
      The_Orb := Orb;
   end Register_Orb;

   procedure Run is
   begin
      if The_Orb = null then
         return;
      else
         Run (The_Orb.all);
      end if;
   end Run;

   procedure Poa_State_Changed (Poa : Broca.Poa.POA_Object_Access) is
   begin
      Poa_State_Changed (The_Orb.all, Poa);
   end Poa_State_Changed;
end Broca.Orb;


