-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----  This package corresponds to the C class IOP defined in file  ----
----  IOP.h. It provides the type Tagged_Profile_List and some     ----
----  methods to marshall and unmarshall it.                       ----
----                                                               ----
----                                                               ----
----                  package iop                                  ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 03/09/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with System ;
with Netbufferedstream ;
with Membufferedstream ;

package Iop is

   type Tagged_Profile_List is new System.Address ;
   -- corresponds to IOP::TaggedProfileList (see IOP.h)
   -- This object is never used in Ada (just taken from a C function
   -- and given to another one) so it is not right implemented.
   -- We just keep the system Address of the object.


   procedure C_Marshall (A : in System.Address ;
                         S : in out System.Address) ;
   pragma Import (C,C_Marshall,"__ars__Q23IOP13TaggedProfileR17NetBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator >>=
   -- (see IOP.h)

   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Netbufferedstream.Object);
   -- Ada equivalent of C procedure C_Marshall


   procedure C_UnMarshall (A : out System.Address ;
                           S : in out System.Address) ;
   pragma Import (C,C_UnMarshall,"__als__Q23IOP13TaggedProfileR17NetBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator <<=
   -- (see IOP.h)

   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Netbufferedstream.Object);
   -- Ada equivalent of C procedure C_UnMarshall


   procedure C_Marshall2 (A : in System.Address ;
                         S : in out System.Address) ;
   pragma Import (C,C_Marshall2,"__ars__Q23IOP13TaggedProfileR17MemBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator >>=
   -- (see IOP.h)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Membufferedstream.Object);
   -- Ada equivalent of C procedure C_Marshall2


   procedure C_UnMarshall2 (A : out System.Address ;
                           S : in out System.Address) ;
   pragma Import (C,C_UnMarshall2,"__als__Q23IOP13TaggedProfileR17MemBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator <<=
   -- (see IOP.h)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Membufferedstream.Object);
   -- Ada equivalent of C procedure C_UnMarshall2

end Iop ;
