with CORBA; use CORBA;
with Broca.Buffers; use Broca.Buffers;

package Broca.IOR is

   function IOR_String_To_Buffer (IOR : CORBA.String) return Buffer_Descriptor;
   --  Convert a string representing an IOR, as decribed in CORBA V2.2
   --  11.6.6, into a buffer ready for unmarshalling.

   function Buffer_To_IOR_String
     (Buffer : Buffer_Descriptor)
     return CORBA.String;
   --  Convert a buffer containing a marshalled IOR into a string.

end Broca.IOR;
