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
----   authors : Sebastien Ponce                                   ----
----   date    : 03/09/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with System.Address_To_Access_Conversions ;

package body Iop is

   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Netbufferedstream.Object) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall (C_A,C_S) ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Netbufferedstream.Object) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_S := S'Address ;
      -- ... calls the C procedure ...
      C_UnMarshall (C_A,C_S) ;
      -- ... and transforms the result in an Ada type
      A := IOP.Tagged_Profile_List (C_A) ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Membufferedstream.Object) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall2 (C_A,C_S) ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Membufferedstream.Object) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_S := S'Address ;
      -- ... calls the C procedure ...
      C_UnMarshall2 (C_A,C_S) ;
      -- ... and transforms the result in an Ada type
      A := IOP.Tagged_Profile_List (C_A) ;
   end ;

end Iop ;
