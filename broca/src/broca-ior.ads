with CORBA; use CORBA;
with Broca.Types; use Broca.Types;

package Broca.Ior is
   --  Convert a string representing an IOR, as decribed in CORBA V2.2 11.6.6,
   --  into a buffer ready for unmarshalling.
   function Ior_String_To_Buffer (Str : CORBA.String) return Buffer_Access;

   --  Convert a buffer containing an marshalled contents of an IOR into
   --  a string.
   function Buffer_To_Ior_String (Buffer : Buffer_Descriptor)
                                  return CORBA.String;
end Broca.Ior;
