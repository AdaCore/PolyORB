-----------------------------------------------------------------------
----                                                               ----
----                  package sys_dep                              ----
----                                                               ----
----                                                               ----
----   This package defines some system dependent types.           ----
----   The system dependence is mostly due to the C++              ----
----   interfacing.                                                ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Interfaces.C ; use Interfaces.C;
with Ada.Text_IO ;

package Sys_Dep is

#if HAS_Cplusplus_Bool
  subtype C_Boolean is Interfaces.CPP.Bool ;
  C_True : C_Boolean := Interfaces.CPP.True ;
  C_False : C_Boolean := Interfaces.CPP.False ;
#else
  subtype C_Boolean is Interfaces.C.Unsigned_Char ;
  C_False : C_Boolean := 0 ;
  -- C_True is private because of its changing definition :
  -- True is <> 0
#end if;
-- Definition of C_Boolean, the Ada equivalent of the C++
-- bool type. Needed to interface C functions in Ada

function Boolean_C_To_Ada (C_Bool : C_Boolean) return Boolean;
function Boolean_Ada_To_C (Bool : Boolean) return C_Boolean;
-- conversion of C_Boolean into Ada boolean and vice versa

private

#if HAS_Cplusplus_Bool
#else
  C_True : C_Boolean := 1 ;
#end if;



end Sys_Dep ;



