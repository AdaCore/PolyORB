--  This package corresponds to the C class IOP defined in file IOP.h. It
--  provides the type Tagged_Profile_List and some methods to marshall and
--  unmarshall it.

with System;
with Interfaces.C;

with NetBufferedStream;
with MemBufferedStream;

with CORBA;

package IOP is

   type Tagged_Profile_List is new System.Address;
   --  Corresponds to IOP::TaggedProfileList* (see IOP.h) This object is
   --  never used in Ada (just taken from a C function and given to another
   --  one) so it is not right implemented.  We just keep the system
   --  Address of the object.


   procedure Marshall
     (A : in IOP.Tagged_Profile_List;
      S : in out NetBufferedStream.Object'Class);

   pragma Import
     (CPP, Marshall,
      "marshall__FPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileR21Ada_netBufferedStream");
   --  Wrapper around Ada_IOP method marshall (see Ada_IOP.h) Marshalls a
   --  Tagged_Profile_List into a NetBufferedStream

   procedure Unmarshall
     (A : in out IOP.Tagged_Profile_List;
      S : in out NetBufferedStream.Object'Class);

   pragma Import
     (CPP, Unmarshall,
      "unmarshall__FRPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileR21Ada_netBufferedStream");
   --  Wrapper around Ada_IOP method marshall (see Ada_IOP.h) Unmarshalls a
   --  Tagged_Profile_List from a NetBufferedStream

   procedure Marshall
     (A : in IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class);
   --  Marshalls a Tagged_Profile_List into a MemBufferedStream

   procedure Unmarshall
     (A : in out IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class);
   --  Unmarshalls a Tagged_Profile_List from a MemBufferedStream

   function Align_Size
     (A             : in IOP.Tagged_Profile_List;
      Initialoffset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Computes the size needed to marshall a Tagged_Profile_List and add
   --  it to the Initialoffset

   function Length
     (A : in IOP.Tagged_Profile_List)
      return CORBA.Unsigned_Long;
   --  Computes the length of a Tagged_Profile_List

end IOP;
