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
----                                                               ----
----     As for now, this package is implemented using a           ----
----     list. It would probably be more efficient with a          ----
----     hashtable.                                                ----
----                                                               ----
----     THE FUNCTIONS ARE NOT THREAD-SAFE                         ----
----                                                               ----
----                   package Corba.Dynamic_Type                  ----
----                                                               ----
----   authors : Fabien Azavant                                    ----
----   date    : 03/09/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba.Object ;

package body Corba.Dynamic_Type is


   type Cell ;
   type Cell_Ptr is access all Cell ;
   type Cell is  record
      ID : Corba.String ;
      Value : Corba.Object.Constant_Ref_Ptr ;
      Next : Cell_Ptr ;
   end record ;

   Pd_List : Cell_Ptr := null ;
   -- This is a static list that contains all the
   -- pairs (repoID, static object ref)

   -- Free : free the memory
   procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_Ptr) ;


   -- Register :
   -------------
   procedure Register (RepoId : in Corba.String ;
                      Dyn_Type : in Corba.Object.Constant_Ref_Ptr) is
      Temp : Cell_Ptr ;
   begin
      -- makes a new cell ...
      Temp := new Cell'(ID => RepoID,
                        Value => Dyn_Type,
                        Next => Pd_list) ;
      -- ... and add it in front of the list
      Pd_List := Temp ;
   end ;


   -- Get_Dynamic_Type_From_Repository_Id
   --------------------------------------
   function Get_Dynamic_Type_From_Repository_Id(RepoID : in Corba.String)
                                                return Corba.Object.Constant_Ref_Ptr is
      Temp : Cell_Ptr := Pd_List ;
   begin
      loop
         if Temp = null then
            Ada.Exceptions.Raise_Exception (AdaBroker_Fatal_Error'Identity,
                                            "Corba.Get_Dynamic_Type_From_Repository_Id"
                                            & Corba.CRLF
                                            & "No match found for "
                                            & Corba.To_Standard_String(RepoId)) ;
         else if Temp.all.ID = repoID then
            return Temp.all.Value ;
         else
            Temp := Temp.all.Next ;
         end if ;
         end if ;
      end loop ;
   end ;


end  Corba.Dynamic_Type ;
