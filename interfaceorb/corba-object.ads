-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                   package Corba.Object                        ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----   This package corresponds to the CORBA 2.0 specification.    ----
----   It contains the definition of type Corba.Object.Ref,        ----
----   which is the base class of all proxy objects                ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Finalization ;

with Omniobject ;
with NetBufferedStream, MemBufferedStream ;

with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;


package Corba.Object is


   Debug : constant Boolean := Adabroker_Debug.Is_Active("corba.object") ;
   Debug_Fin : constant Boolean := Adabroker_Debug.Is_Active("corba.object:finalization") ;

   --------------------------------------------------
   ---        CORBA 2.2 specifications            ---
   --------------------------------------------------

   type Ref is tagged private ;
   type Ref_Ptr is access all Ref'Class ;
   type Constant_Ref_Ptr is  access constant Corba.Object.Ref'Class ;

   -- constant Nil Ref
   Nil_Ref : aliased constant Ref ;


   function Is_Nil(Self: in Ref) return Corba.Boolean;
   function Is_Null(Self: in Ref) return Corba.Boolean renames Is_Nil;
   -- returns nil if the ORB is sure that this is a nil object
   -- ie, it points nowhere


   procedure Release (Self : in out Ref'class);
   -- releases all the resources this proxy object
   -- uses.


   -- Object duplicate() is not needed in Ada
   -- we use assignment according to the CORBA 2.0 specification


   function Is_A(Self: in Ref ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean ;
   -- returns true if this object is of this Logical_Type_Id
   -- ( where Logical_Type_Id is a Repository_Id )
   -- or one of its descendants


   function Is_A(Logical_Type_Id : in Corba.String)
                 return Corba.Boolean ;
   -- same, but non dispatching, must be called
   -- Corba.Object.Is_A(...)


   function Non_Existent(Self : in Ref) return Corba.Boolean ;
   -- returns true if the ORB knows that the implementation
   -- referenced by this proxy object does not exist


   function Is_Equivalent(Self : in Ref ;
                          Other : in Ref)
                          return Corba.Boolean ;
   -- returns True if both objects point to the same
   -- distant implementation


   function Hash(Self : in Ref ;
                 Maximum : in Corba.Unsigned_long)
                 return Corba.Unsigned_Long ;
   -- return a hash value for this object
   -- not implemented yet, it returns 0


   -- get_implementation and get_interface
   -- are not supported


   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;
   -- used to cast any Corba.Object.Ref'Class into a Corba.Object.Ref
   -- it has to be overloaded for all descendants of Corba.Object.Ref


   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------

   Repository_Id : Corba.String := Corba.To_Corba_String("IDL:omg.org/CORBA/Object:1.0") ;
   -- Repository Id for Corba.Object.Ref

   function Get_Repository_Id(Self : in Ref) return Corba.String ;
   -- returns the Repository Id of the ADA type of Self
   -- it must be overloaded for all the descendants of Ref
   -- BEWARE : the repository ID of the ADA type of
   -- Self may not be the same as its dynamic repository ID
   -- For example : if a descendant Myref has been cast into
   -- Ref using To_Ref, Get_Repository_Id will return
   -- the repository ID of Corba.Object.Ref


   function Get_Nil_Ref(Self : in Ref) return Ref ;
   -- this function returns a Nil_Ref of the same type
   -- of the object it is given. It is used for
   -- typing (see below)


   -- function Get_OmniObject_Ptr (Self : in Ref) return Omniobject.Object_Ptr ;
   -- return the underlying omniobject

   function Get_Omniobject_Ptr(Self : in Ref'Class)
                               return Omniobject.Object_Ptr ;
   -- returns the underlying Omniobject.Object

   procedure Internal_Copy(From : in Ref'Class ;
                           To : in out Ref'Class) ;
   -- This is a workaround for a bug in gnat 3.11p
   -- it simply copies the two fields of From
   -- into To. It also finalizes and adjusts whrn needed

   -----------------------------------------------
   --             object <-> IOR                --
   -----------------------------------------------

   function Object_To_String (Self : in CORBA.Object.Ref'class)
                              return CORBA.String ;
   -- returns the IOR corresponding to this object
   -- it is called by Corba.Orb.Object_To_String
   -- see CORBA specification for details

   procedure String_to_Object (From : in CORBA.String;
                               To : out Ref'class) ;
   -- returns a Ref'Class out of an IOR
   -- it is called by Corba.Orb.String_To_Object
   -- see CORBA specification for details


   --------------------------------------------------
   ---   registering new interfaces into the ORB  ---
   --------------------------------------------------

   procedure Create_Proxy_Object_Factory(RepoID : in Corba.String) ;
   -- The ORB has to know how to create new proxy objects
   -- when we ask him to (out of an IOR for example.
   -- To do that, it keeps a global variable which
   -- is a list of proxyObjectFactories.
   -- They all have a method newProxyObject
   -- which construct a proxy object of the desired type
   --
   -- From the ORB's point of view, all Ada objects
   -- are of the same type, all we have to do is register
   -- the Repositoty ID of the new interface, this is
   -- done in this function, which has to be called
   -- in the elaboration of all the packages that contain
   -- a descendant of Corba.Object.Ref



-----------------------------------------------------------------------
----                                                               ----
----          dynamic typing of objects                            ----
----                                                               ----
----    An omniobject.Object carries                               ----
----   information about its most derived type in its reposituryID ----
----     In Corba.Object.Ref, the information of the most          ----
----   derived type is stored as a pointer to a static variable    ----
----   of the most derived clas of the object. (Nil_Ref is used)   ----
----                                                               ----
----   Ada typing cannot be used, because the Ada dynamic          ----
----   type does not correspond to the IDL dynamic type            ----
----   when To_Ref is used.                                        ----
----                                                               ----
----    It is easy to retrieve the repositoryID out of a static    ----
----   object via the dispatching method Get_Repository_Id,        ----
----   and this package provides a function                        ----
----   Get_Dynamic_Type_From_Repository_Id                         ----
----   to do the conversion the other way round.                   ----
----                                                               ----
----                                                               ----
----     As for now, this package is implemented using a           ----
----     list. It would probably be more efficient with a          ----
----     hashtable.                                                ----
----                                                               ----
----     THE FUNCTIONS ARE NOT THREAD-SAFE                         ----
----                                                               ----
-----------------------------------------------------------------------


   procedure Register(RepoId : in Corba.String ;
                      Dyn_Type : in Corba.Object.Constant_Ref_Ptr) ;
   -- this procedure registers a new static object in the list


   function Get_Dynamic_Type(Self: in Ref) return Ref'Class ;
   -- returns a Ref_Ptr which is of the same class
   -- as the most derived repository id of this Ref'Class



   --------------------------------------------------
   ---       Marshalling operators                ---
   --------------------------------------------------

   function Align_Size (Obj : in Ref'Class ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;
   -- This function computes the size needed to marshall the object obj

   procedure Marshall (Obj : in Ref'Class ;
                       S : in out NetBufferedStream.Object'Class) ;
   -- This procedure marshalls the object Obj into the stream S

   procedure Marshall (Obj : in Ref'Class ;
                       S : in out MemBufferedStream.Object'Class) ;
   -- This procedure marshalls the object Obj into the stream S

   procedure UnMarshall (Obj : out Ref'Class ;
                         S : in out NetBufferedStream.Object'Class) ;
   -- This procedure marshalls the object Obj into the stream S

   procedure UnMarshall (Obj : out Ref'Class ;
                         S : in out MemBufferedStream.Object'Class) ;
   -- This procedure marshalls the object Obj into the stream S




private

   --------------------------------------------------
   ---     references to implementations          ---
   --------------------------------------------------

   type Ref is new Ada.Finalization.Controlled with record
      Omniobj : Omniobject.Object_Ptr := null ;
      Dynamic_Type : Constant_Ref_Ptr := null ;
   end record ;


   --------------------------------------------------
   ---          controlling functions             ---
   --------------------------------------------------

   procedure Initialize(Self : in out Ref) ;
   -- sets Omniobj and Dynamic_Type to null ;

   procedure Adjust (Self: in out Ref);
   -- duplicate the underlying omniobject


   procedure Finalize (Self: in out Ref);
   -- release th underlying omniobject


   Nil_Ref :  aliased constant Ref := (Ada.Finalization.Controlled
                                       with Omniobj => null,
                                       Dynamic_Type => null) ;

end Corba.Object ;
