with Broca.Orb;
with Broca.Refs;

package body Broca.Marshalling.Refs is

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Stream : in out Buffer_Descriptor;
      Value  : in CORBA.Object.Ref'Class)
   is
      IOR_Buffer : Buffer_Descriptor
        :=  Broca.Refs.Object_To_IOR (CORBA.Object.Get (Value).all);
   begin
      Append_Buffer (Stream, IOR_Buffer);
   end Marshall;

   -------------------
   -- Marshall_Size --
   -------------------

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor;
      Value  : in CORBA.Object.Ref'Class)
   is
      IOR_Buffer : Buffer_Descriptor
        :=  Broca.Refs.Object_To_IOR (CORBA.Object.Get (Value).all);
   begin
      Compute_New_Size (Stream, IOR_Buffer);
   end Marshall_Size;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor;
      Result : out CORBA.Object.Ref'Class)
   is
       New_Ref : CORBA.Object.Ref;
   begin
     Broca.Orb.IOR_To_Object (Stream, New_Ref);
     Broca.Refs.Set (Broca.Refs.Ref (Result), Broca.Refs.Get
                     (Broca.Refs.Ref (New_Ref)));
   end Unmarshall;

end Broca.Marshalling.Refs;

