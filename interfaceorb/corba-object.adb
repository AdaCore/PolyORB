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


with Ada.Exceptions ;

with Iop ;
with Omniobject ;
use type Omniobject.Object_Ptr ;
with Omniropeandkey ; use Omniropeandkey ;

with Adabroker_Debug ; use Adabroker_Debug ;

package body Corba.Object is

   --------------------------------------------------
   ---        CORBA 2.2 specifications            ---
   --------------------------------------------------

   -- Is_Nil
   ---------
   function Is_Nil(Self: in Ref'Class) return Boolean is
   begin
      return  Self.Omniobj = null ;
   end ;


   -- Release
   ----------
   procedure Release (Self : in out Ref'class) is
   begin
      Finalize(Self) ;
   end ;


   -- Is_A
   -------
   function Is_A(Self: in Ref ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean is
   begin
      return (Repository_Id = Logical_Type_Id) ;
   end ;


   -- Is_A
   -------
   function Is_A(Logical_Type_Id : in Corba.String)
                 return Corba.Boolean is
   begin
      return (Repository_Id = Logical_Type_Id) ;
   end ;


   -- Non_Existent
   ---------------
   function Non_Existent(Self : in Ref) return Corba.Boolean is
   begin
      if Is_Nil(self) then
         return True ;
      end if ;
      return Omniobject.Non_Existent(Self.Omniobj.all) ;
   end ;


   -- Is_Equivalent
   ----------------
   function Is_Equivalent(Self : in Ref ;
                          Other : in Ref)
                          return Corba.Boolean is
      Rak : Omniropeandkey.Controlled_Wrapper ;
      Other_Rak : Omniropeandkey.Controlled_Wrapper ;
      S1, S2 : Corba.Boolean ;
   begin
      -- this is copied from corbaObject.cc L160.
      -- Here, Refs are proxy objects
      OmniObject.Get_Rope_And_Key(Self.Omniobj.all, Rak.Real, S1) ;
      Omniobject.Get_Rope_And_Key(Other.Omniobj.all, Other_Rak.Real, S2) ;
      return S1 and S2 and (Rak.Real = Other_Rak.Real) ;
   end;


   -- Hash
   -------
   function Hash(Self : in Ref ;
                 Maximum : in Corba.Unsigned_long)
                 return Corba.Unsigned_Long is
   begin
      if Is_Nil(Self) then
         Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                        "Cannot call function hash on a nil reference") ;
      end if ;
      return Omniobject.Hash(Self.Omniobj.all, Maximum) ;
   end ;


    -- To_Ref
    ---------
    function To_Ref(The_Ref : in Ref'Class) return Ref is
    begin
       return Ref(The_Ref) ;
    end ;


   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------

   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Ref) return Corba.String is
   begin
      return Repository_Id ;
   end ;


   -- Get_Nil_Ref
   --------------
   function Get_Nil_Ref(Self : in Ref) return Ref is
   begin
      return Nil_Ref ;
   end ;


   -- Get_OmniObject_Ptr
   ---------------------
   function Get_OmniObject_Ptr (Self : in Ref'Class)
                                return Omniobject.Object_Ptr is
   begin
      return Self.Omniobj ;
   end ;


   -- Object_To_String
   -------------------
   function Object_To_String (Self : in Ref'class)
                              return CORBA.String is
   begin
      if Is_Nil(Self) then
         return Omniobject.Object_To_String(null) ;
      else
         return Omniobject.Object_To_String(Self.Omniobj) ;
      end if ;
   end ;


   -- String_To_Object
   -------------------
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class) is
      RepoId : Corba.String ;
      Most_Derived_Type : Constant_Ref_Ptr ;
   begin
      -- Get the omniobject
      To.Omniobj:= Omniobject.String_To_Object(From) ;

      -- if the result is correct
      if not (To.Omniobj = null) then

         -- check if the omniobject we got can be put into
         -- To (type implied the repoId)
         RepoId := Omniobject.Get_Repository_Id(To.Omniobj.all) ;

         pragma Debug(Output(Debug,
                             "Corba.Object.String_To_Object : repoid = "
                             & Corba.To_Standard_String(RepoId))) ;

         Most_Derived_Type := Get_Dynamic_Type_From_Repository_Id(Repoid) ;
         -- dyn_type is now an object of the most derived type
         -- of the new created object

         if Is_A(Most_Derived_Type.all, Get_Repository_Id(To)) then
            To.Dynamic_Type := Most_Derived_Type ;
            return ;
         end if ;
      end if ;

      -- otherwise, the operation is illegal return Nil_Ref
      -- in the right class
      To.Omniobj := null ;
      To.Dynamic_Type := null ;

   end ;


   -- Object_Is_Ready
   ------------------
   procedure Object_Is_Ready(Self : in Ref'Class) is
   begin
      if not Is_Nil(Self) then
         Omniobject.Omniobject_Is_Ready(Self.Omniobj) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Corba.Object.Object_Is_Ready(Corba.Object.Ref)"
                                        & Corba.CRLF
                                        & "Cannot be called on nil object") ;
      end if ;
   end ;


   --  Get_Dynamic_Type
   ----------------------
    function Get_Dynamic_Type(Self: in Ref) return Ref'Class is
    begin
       return Self.Dynamic_Type.all ;
    end ;


   --------------------------------------------------
   ---   registering new interfaces into the ORB  ---
   --------------------------------------------------


    -- C_Create_Proxy_Object_Factory
   ---------------------------------
   procedure C_Create_Proxy_Object_Factory(RepoID : in Interfaces.C.Strings.Chars_ptr) ;
   pragma Import (CPP, C_Create_Proxy_Object_Factory, "createProxyObjectFactory__FPCc") ;
   -- corresponds to
   -- void createProxyObjectFactory(const char* repoID)
   -- see proxyObjectFactory.hh


   -- Create_Proxy_Object_Factory
   ------------------------------
   procedure Create_Proxy_Object_Factory(RepoID : in Corba.String) is
      C_Repoid : Interfaces.C.Strings.Chars_Ptr ;
      Tmp : Standard.String := Corba.To_Standard_String(RepoId) ;
   begin
      C_Repoid := Interfaces.C.Strings.New_String(Tmp) ;
      -- never deallocated because it is stored in a global
      -- variable in omniORB (proxyStubs)
      C_Create_Proxy_Object_Factory(C_RepoId) ;
   end ;


    --------------------------------------------------
    ---     dynamic typing of objects              ---
    --------------------------------------------------


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


   --------------------------------------------------
   ---       Marshalling operators                ---
   --------------------------------------------------


   -- Create_Ref
   -------------
   procedure Create_Ref(Most_Derived_Repoid : in Corba.String ;
                        Profiles : in Iop.Tagged_Profile_List ;
                        Release : in Corba.Boolean ;
                        To : in out Ref'Class ) is
      Most_Derived_Type : Constant_Ref_Ptr ;
   begin
      -- check if the omniobject we got can be put into
      -- To (type implied the repoId)
         Most_Derived_Type := Get_Dynamic_Type_From_Repository_Id(Most_Derived_Repoid) ;
         -- most_derived_type is now an object of the most derived type
         -- of the new created object
      if Is_A(Most_Derived_Type.all, Get_Repository_Id(To)) then

         -- Get the omniobject
         To.Omniobj := Omniobject.Create_Omniobject(Most_Derived_Repoid,
                                                    Profiles,
                                                    Release) ;
         -- if the result is correct
         if not (To.Omniobj = null) then

            To.Dynamic_Type
              := Get_Dynamic_Type_From_Repository_Id(Most_Derived_Repoid) ;
            return ;
         end if ;
      end if ;

      -- otherwise, the operation is illegal return Nil_Ref
      -- in the right class
      To.Omniobj := null ;
      To.Dynamic_Type := null ;

   end ;


   -- Get_Profile_List
   -------------------
   function Get_Profile_List (Obj : in Ref'Class)
                              return Iop.Tagged_Profile_List is
   begin
      return Omniobject.Get_Profile_List (Obj.Omniobj.all) ;
      -- calls the corresponding function on the underlying omniobject
   end ;


   -- Align_Size
   -------------
   function Align_Size (Obj : in Ref_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
   begin
      if Obj = null then
         -- never reached normally
         Ada.Exceptions.Raise_Exception (AdaBroker_Fatal_Error'Identity ,
                                     "Null pointer argument in function Align_Size in corba-object.") ;
      else
         -- calls the corresponding function on the underlying omniobject
         return Omniobject.Align_Size (Obj.all.Omniobj, Initial_Offset) ;
      end if ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (Obj : in Ref_Ptr ;
                       S : in out NetBufferedStream.Object'Class) is
   begin
      if Obj = null then
         -- never reached normally
         Ada.Exceptions.Raise_Exception (AdaBroker_Fatal_Error'Identity ,
                                     "Null pointer argument in procedure Marshall in corba-object.") ;
      else
         -- calls the corresponding function on the underlying omniobject
         Omniobject.Marshall (Obj.all.Omniobj,S) ;
      end if ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (Obj : in Ref_Ptr ;
                       S : in out MemBufferedStream.Object'Class) is
   begin
      if Obj = null then
         -- never reached normally
         Ada.Exceptions.Raise_Exception (AdaBroker_Fatal_Error'Identity ,
                                     "Null pointer argument in procedure Marshall in corba-object.") ;
      else
         -- calls the corresponding function on the underlying omniobject
         Omniobject.Marshall (Obj.all.Omniobj,S) ;
      end if ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (Obj : out Ref'Class ;
                         S : in out NetBufferedStream.Object'Class) is
      Repo_ID : Corba.String ;
      Iop_List : Iop.Tagged_Profile_List ;
      Tmp : Ref'Class := Corba.Object.Nil_Ref ;
   begin
      -- first unmarshall the Repo_Id
      NetBufferedStream.UnMarshall (Repo_ID,S) ;
      -- then the profile list
      Iop.UnMarshall (Iop_List,S) ;
      -- and at last create the object reference to be returned
      if (Iop.Length(Iop_List) = Corba.Unsigned_Long (0))
        and (Corba.Length (Repo_ID) = Corba.Unsigned_Long (0)) then
         -- either a nil object reference
         Obj := Tmp ;
      else
         -- or a real object reference
         Corba.Object.Create_Ref (Repo_ID,Iop_List,True,Obj) ;
      end if ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (Obj : out Ref'Class ;
                         S : in out MemBufferedStream.Object'Class) is
      Repo_ID : Corba.String ;
      Iop_List : Iop.Tagged_Profile_List ;
      Tmp : Ref'Class := Corba.Object.Nil_Ref ;
   begin
      -- first unmarshall the Repo_Id
      MemBufferedStream.UnMarshall (Repo_ID,S) ;
      -- then the profile list
      Iop.UnMarshall (Iop_List,S) ;
            -- and at last create the object reference to be returned
      if Iop.Length(Iop_List) = Corba.Unsigned_Long (0)
        and Corba.Length (Repo_ID) = Corba.Unsigned_Long (0) then
         -- either a nil object reference
         Obj := Tmp ;
      else
         -- or a real object reference
         Corba.Object.Create_Ref (Repo_ID,Iop_List,True,Obj) ;
      end if ;
   end ;



   --------------------------------------------------
   ---          controlling functions             ---
   --------------------------------------------------

   -- Adjust
   ---------
   procedure Adjust (Self: in out Ref) is
   begin
      if not Is_Nil(Self) then
         Self.Omniobj := Omniobject.Omniobject_Duplicate(Self.Omniobj) ;
      end if ;
   end ;


   -- Finalize
   -----------
   procedure Finalize (Self: in out Ref) is
   begin
      if not Is_Nil(Self) then
         Omniobject.Omniobject_Destructor(Self.Omniobj) ;
         Self.Omniobj := null ;
         Self.Dynamic_Type := null ;
      end if ;
   end ;




begin

   Register(Repository_Id, Nil_Ref'Access) ;
   -- registers the fact that a new IDL interface : the root of all the others
   -- can be used in the program

end Corba.Object ;










