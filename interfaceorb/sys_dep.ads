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

with Interfaces.C ;

package Sys_Dep is

#if HAS_Cplusplus_Bool
     subtype C_Boolean is Interfaces.CPP.Bool ;
#else
     subtype C_Boolean is Interfaces.C.Unsigned_Char ;
#end if;

end Sys_Dep;



