with Broca.Orb;
with Broca.Refs;
 
package body Broca.Marshalling.Refs is
 
  procedure Unmarshall
    (Stream : in out Buffer_Descriptor; Res : out CORBA.Object.Ref'Class)
  is
     New_Ref : CORBA.Object.Ref;
  begin
     Broca.Orb.IOR_To_Object (Stream, New_Ref);
     Broca.Refs.Set (Broca.Refs.Ref (Res), Broca.Refs.Get
                     (Broca.Refs.Ref (New_Ref)));
  end Unmarshall;

  procedure Marshall_Size
    (Stream : in out Buffer_Descriptor; Val : CORBA.Object.Ref'Class) is
     IOR_Buffer : Buffer_Descriptor
       :=  Broca.Refs.Object_To_IOR (CORBA.Object.Get (Val).all);
  begin
     Marshall_Size_Append (Stream, IOR_Buffer);
  end Marshall_Size;

  procedure Marshall
    (Stream : in out Buffer_Descriptor; Val : CORBA.Object.Ref'Class) is
     IOR_Buffer : Buffer_Descriptor
       :=  Broca.Refs.Object_To_IOR (CORBA.Object.Get (Val).all);
  begin
     Marshall_Append (Stream, IOR_Buffer);
  end Marshall;

end Broca.Marshalling.Refs;
