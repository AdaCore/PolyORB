-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is a sub package of package corba dealing    ----
----   with Corba exceptions.                                      ----
----     It provides two main functions : Raise_corba_Exception    ----
----   and Get_Members. These functions allows the programmer to   ----
----   associate to each exception a "memmber" structure with      ----
----   all kinds of datas he needs.                                ----
----                                                               ----
----                                                               ----
----                   package Corba.Exceptions                    ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 03/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package Corba.Exceptions is

   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence ;
                          To : out Ex_Body) ;
   -- This method finds the member object associated to a given exception.

   procedure Raise_Corba_Exception(Excp : in Ada.Exceptions.Exception_Id ;
                                   Excp_Memb: in Idl_Exception_Members_Ptr) ;
   -- This method raises a Corba exception associated to the member object
   -- Excp_Memb.

end Corba.Exceptions ;
