-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is a sub package of package corba dealing    ----
----   with dynamic type checking.                                 ----
----     As a matter of fact, an omniobject.Object carries         ----
----   information about its most derived type in its reposituryID ----
----     In Corba.Object.Ref, the information of the most          ----
----   derived type is stored as a pointer to a static variable    ----
----   of the most derived clas of the object.                     ----
----    It is easy to retrieve the repositoryID out of a static    ----
----   object via the dispatching method Get_Repository_Id,        ----
----   and this package provides a function Get_Dynamic_Type       ----
----   to do the conversion the other way round.                   ----
----                                                               ----
----     As for now, this package is implemented using a           ----
----     list. It would probably be more efficient with a          ----
----     hashtable.                                                ----
----                                                               ----
----     THE FUNCTIONS ARE NOT THREAD-SAFE                         ----
----                                                               ----
----                                                               ----
----                   package Corba.Dynamic_Type                  ----
----                                                               ----
----   authors : Fabien Azavant                                    ----
----   date    : 03/09/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba ;
with Corba.Object ;

package Corba.Dynamic_Type is


   function Get_Dynamic_Type_From_Repository_Id(RepoID : in Corba.String)
                                                return Corba.Object.Ref'Class ;
   -- This function takes a repository_id as input
   -- and returns the static nil Ref of the class corresponding
   -- to this repository_id

   procedure Register(RepoId : in Corba.String ;
                      Dyn_Type : in Corba.Object.Ref_Ptr) ;
   -- this procedure registers a new static object in the list

end ;
