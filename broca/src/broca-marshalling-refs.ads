with CORBA.Object;
 
package Broca.Marshalling.Refs is
 
  procedure Unmarshall
    (Stream : in out Buffer_Descriptor; Res : out CORBA.Object.Ref'Class);

  procedure Marshall_Size
    (Stream : in out Buffer_Descriptor; Val : CORBA.Object.Ref'Class);

  procedure Marshall
    (Stream : in out Buffer_Descriptor; Val : CORBA.Object.Ref'Class);

end Broca.Marshalling.Refs;
