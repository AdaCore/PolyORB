-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class GIOP         ----
----   declared in GIOP.h.                                         ----
----     It provides some Ada equivalents of C++ types and the     ----
----   corresponding translation fonctions.                        ----
----                                                               ----
----                  package body giop                            ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Ada.Unchecked_Conversion ;

package body Giop is

   -- Ada_To_C_Int
   ---------------
   function Ada_To_C_Int is
     new Ada.Unchecked_Conversion (Integer,
                                   Interfaces.C.Int) ;
   -- needed to change ada type Integer
   -- into C type Interfaces.C.Int


   -- Reply_Status_Type_To_C_Int
   -----------------------------
   function Reply_Status_Type_To_C_Int (Status : in Reply_Status_Type)
                                        return Interfaces.C.Int is
   begin
      return Ada_To_C_Int (Reply_Status_Type'Pos(Status)) ;
   end;


   -- C_Int_To_Reply_Status_Type
   -----------------------------
   function C_Int_To_Reply_Status_Type (N : in Interfaces.C.Int)
                                        return Reply_Status_Type is
   begin
      return Reply_Status_Type'Val(N) ;
   end;


end Giop ;
