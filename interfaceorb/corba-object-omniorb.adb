------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                 C O R B A . O B J E C T . O M N I O R B                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.11 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Tags; use Ada.Tags;

with Interfaces.C;         use  Interfaces.C;
with Interfaces.C.Strings;

with CORBA.Object;

with AdaBroker.IOP;     use AdaBroker.IOP;
with AdaBroker.OmniORB; use AdaBroker.OmniORB;

with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

package body CORBA.Object.OmniORB is

   Flag : constant Natural
     := AdaBroker.Debug.Is_Active ("corba.object.omniorb");
   procedure O is new AdaBroker.Debug.Output (Flag);

   type Interface is record
      The_Ref  : Ref_Ptr;
      The_Rep  : CORBA.String;
      The_Tag  : CORBA.String;
      Dispatch : Dispatch_Procedure;
   end record;

   Table : array (int range 1 .. 256) of Interface;
   Last  : int := 0;
   Base  : int := 0;

   procedure C_Create_Proxy_Object_Factory
     (Repository : in Strings.chars_ptr;
      Interface  : in int);

   pragma Import
     (CPP, C_Create_Proxy_Object_Factory, "createProxyObjectFactory__FPCci");
   --  Correspond to void createProxyObjectFactory (const char* repoID) see
   --  proxyObjectFactory_C2Ada.hh

   procedure Adjust_Object_To_Type
     (RepoID   : in CORBA.String;
      Profiles : in Tagged_Profile_List;
      Result   : in out Ref'Class);

   ---------------------------
   -- Adjust_Object_To_Type --
   ---------------------------

   procedure Adjust_Object_To_Type
     (RepoID   : in CORBA.String;
      Profiles : in Tagged_Profile_List;
      Result   : in out Ref'Class)
   is
      Incoming_Ref : Ref_Ptr      := Id_To_Ref (Rep_To_Id (RepoID));

      Outgoing_Rep : CORBA.String := Id_To_Rep (Tag_To_Id (Result));
      --  Warning: Result has no OmniObj reference yet. To compute
      --  Outgoing_Rep, get its id with Tag_To_Id (using Result'Tag).

   begin
      --  Check whether OmniObject can be cast into To type. Incoming_Ref
      --  is an object of the most derived type from RepoID. Outgoing_Rep
      --  is the repository id of the expected output. We must check that
      --  the incoming object is in the hierarchy of the outgoing object.

      if Is_A (Incoming_Ref.all, Outgoing_Rep) then
         Result.OmniObj := Create_OmniObject (RepoID, Profiles);

         --  If the result is correct
         if Result.OmniObj /= null then
            return;
         end if;
      end if;

      --  Illegal conversion.
      pragma Debug
        (O ("Adjust_Object_To_Type : cannot convert " &
            CORBA.To_Standard_String (RepoID) &
            " into " &
            CORBA.To_Standard_String (Outgoing_Rep)));

      Result.OmniObj := null;
   end Adjust_Object_To_Type;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in CORBA.String;
      To   : out CORBA.Object.Ref'Class)
   is
      Source_Ref : Ref_Ptr;
      Target_Rep : CORBA.String;
   begin
      To.OmniObj := AdaBroker.OmniORB.String_To_Object (From);


      if To.OmniObj /= null then

         Source_Ref := Id_To_Ref (To.OmniObj.Interface);
         --  AdaBroker.OmniORB.String_To_Object is supposed to set up
         --  To.OmniObj.Interface because it can retrieve the RepoID.

         Target_Rep := Id_To_Rep (Tag_To_Id (To));
         --  Tag_To_Id uses To'Tag and not To.OmniObj.Interface to
         --  find out the Interface index.

         if Is_A (Source_Ref.all, Target_Rep) then
            return;
         end if;

      end if;

      --  Illegal conversion.
      To.OmniObj := null;
      pragma Debug (O ("To.OmniObj = null"));

   end String_To_Object;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : in CORBA.String)
     return CORBA.Object.Ref
   is
      Result : CORBA.Object.Ref;
   begin
      Result.OmniObj :=
        AdaBroker.OmniORB.Resolve_Initial_References (Identifier);
      Set_Interface_Rep
        (Result.OmniObj.all,
         Get_Rep_From_ORB (Result.OmniObj.all));
      return Result;
   end Resolve_Initial_References;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String
   is
   begin
      if Is_Nil (Obj) then
         return Object_To_String (null);
      else
         return Object_To_String (Obj.OmniObj);
      end if;
   end Object_To_String;

   --------------
   -- Register --
   --------------

   procedure Register
     (The_Rep  : in CORBA.String;
      The_Ref  : in CORBA.Object.Ref'Class;
      Dispatch : in Dispatch_Procedure)
   is
      C_RepoID : Strings.chars_ptr;
      Index    : int;
   begin
      if Dispatch /= null then
         pragma Debug (O ("Register impl of " &
                          To_Standard_String (The_Rep)));
         null;
      else
         pragma Debug (O ("Register ref of " &
                          To_Standard_String (The_Rep)));
         null;
      end if;

      Index := 1;
      while Index <= Last
        and then Table (Index).The_Rep /= The_Rep
      loop
         Index := Index + 1;
      end loop;

      if Index > Last then
         Last := Last + 1;
         Table (Last) := (new Ref'Class'(The_Ref),
                          The_Rep,
                          To_CORBA_String (Expanded_Name (The_Ref'Tag)),
                          Dispatch);

         --  The ORB has to know how to create new proxy objects when
         --  we ask him to do so (out of an IOR for example). It keeps
         --  a global variable which is a list of
         --  proxyObjectFactories. They all have a method
         --  newProxyObject which construct a proxy object of the
         --  desired type

         C_RepoID := Strings.New_String (CORBA.To_Standard_String (The_Rep));

         --  Never deallocated because it is stored in a global
         --  variable in omniORB (proxyStubs).

         C_Create_Proxy_Object_Factory (C_RepoID, Last);

      elsif Table (Index).Dispatch = null then
         Table (Index).Dispatch := Dispatch;
      end if;

      if The_Rep = CORBA.Object.Repository_Id then
         Base := Last;
      end if;
   end Register;

   ---------------
   -- Rep_To_Id --
   ---------------

   function Rep_To_Id  (Self : CORBA.String) return int is
   begin
      for I in 1 .. Last loop
         if Table (I).The_Rep = Self then
            return I;
         end if;
      end loop;
      return 0;
   end Rep_To_Id;

   ---------------
   -- Tag_To_Id --
   ---------------

   function Tag_To_Id  (Self : Ref'Class) return int is
      Tag : CORBA.String := To_CORBA_String (Expanded_Name (Self'Tag));
   begin
      for I in 1 .. Last loop
         if Table (I).The_Tag = Tag then
            return I;
         end if;
      end loop;

      --  In this case, the ref is not registered. It is a forward
      --  reference which is directly derived from CORBA.Object.Ref.
      return Base;
   end Tag_To_Id;

   ---------------
   -- Ref_To_Id --
   ---------------

   function Ref_To_Id  (Self : Ref'Class) return int is
   begin
      if Self.OmniObj /= null then
         return Self.OmniObj.Interface;
      end if;

      --  If Self.OmniObj = null then we have a nil ref. The id of a
      --  nil ref can be found using its tag.
      return Tag_To_Id (Self);
   end Ref_To_Id;

   ---------------
   -- Id_To_Ref --
   ---------------

   function Id_To_Ref  (Self : int) return Ref_Ptr is
   begin
      if 1 <= Self and then  Self <= Last then
         return Table (Self).The_Ref;
      end if;
      raise Constraint_Error;
   end Id_To_Ref;

   ---------------
   -- Id_To_Rep --
   ---------------

   function Id_To_Rep  (Self : int) return CORBA.String is
   begin
      if 1 <= Self and then  Self <= Last then
         return Table (Self).The_Rep;
      end if;
      raise Constraint_Error;
   end Id_To_Rep;

   ---------------
   -- Id_To_Rep --
   ---------------

   function Id_To_Dispatch  (Self : int) return Dispatch_Procedure is
   begin
      if 1 <= Self and then  Self <= Last then
         return Table (Self).Dispatch;
      end if;
      raise Constraint_Error;
   end Id_To_Dispatch;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self   : in ImplObject'Class;
      RepoID : in CORBA.String)
     return CORBA.Object.Ref
   is
      Result     : CORBA.Object.Ref;
   begin
      pragma Debug (O ("To_Ref : enter"));

      Result.OmniObj := Duplicate_OmniObject (Get_OmniObject_Ptr (Self));
      --  Repository     := Get_Rep_From_ORB (Result.OmniObj.all);
      --  Set_Interface_Rep (Result.OmniObj.all, Repository);

      pragma Debug (O ("To_Ref : leave"));

      if Is_A (Id_To_Ref (Result.OmniObj.Interface).all, RepoID) then
         return Result;
      end if;

      Ada.Exceptions.Raise_Exception
        (Constraint_Error'Identity,
         "cannot cast " &
         CORBA.To_Standard_String (Id_To_Rep (Result.OmniObj.Interface)) &
         " into " &
         CORBA.To_Standard_String (RepoID));
   end To_Ref;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Obj            : in Ref'Class;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      --  Call the corresponding function on the underlying omniobject
      return Align_Size (Obj.OmniObj, Initial_Offset);
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out AdaBroker.NetBufferedStream.Object'Class) is
   begin
      --  Call the corresponding function on the underlying omniobject
      AdaBroker.OmniORB.Marshall (Obj.OmniObj, S);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out AdaBroker.MemBufferedStream.Object'Class)
   is
   begin
      --  Call the corresponding function on the underlying omniobject
      AdaBroker.OmniORB.Marshall (Obj.OmniObj, S);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out AdaBroker.NetBufferedStream.Object'Class)
   is
      use Ada.Strings.Unbounded;

      RepoID : CORBA.String;
      List   : Tagged_Profile_List;
   begin
      pragma Debug (O ("unmarshall : enter"));

      --  First unmarshall the Repository
      AdaBroker.NetBufferedStream.Unmarshall (RepoID, S);

      pragma Debug (O ("unmarshall : repoid " & To_Standard_String (RepoID)));

      --  Then the profile list
      AdaBroker.IOP.Unmarshall (List, S);

      --  And at last create the object reference to be returned
      if Length (List) = 0
        and then Length (Unbounded_String (RepoID)) = 0
      then
         pragma Debug (O ("unmarshall : nil ref detected"));

         --  Either a nil object reference
         Obj := Ref'Class (Nil_Ref);
      else
         pragma Debug (O ("unmarshall : find ref"));

         --  Or a real object reference
         Adjust_Object_To_Type (RepoID, List, Obj);

         if Is_Nil (Obj) then
            pragma Debug (O ("unmarshall : nil ref found"));
            null;
         end if;
      end if;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out AdaBroker.MemBufferedStream.Object'Class)
   is
      use Ada.Strings.Unbounded;

      RepoID : CORBA.String;
      List   : Tagged_Profile_List;
   begin
      pragma Debug (O ("unmarshall : enter"));

      --  First unmarshall the Repository
      AdaBroker.MemBufferedStream.Unmarshall (RepoID, S);

      pragma Debug (O ("unmarshall : repoid " & To_Standard_String (RepoID)));

      --  Then the profile list
      AdaBroker.IOP.Unmarshall (List, S);

      --  And at last create the object reference to be returned
      if Length (List) = 0
        and then Length (Unbounded_String (RepoID)) = 0
      then
         pragma Debug (O ("unmarshall : nil ref detected"));

         --  Either a nil object reference
         Obj := Ref'Class (Nil_Ref);
      else
         pragma Debug (O ("unmarshall : find ref"));

         --  Or a real object reference
         Adjust_Object_To_Type (RepoID, List, Obj);

         if Is_Nil (Obj) then
            pragma Debug (O ("unmarshall : nil ref found"));
            null;
         end if;
      end if;
   end Unmarshall;

begin

   Register (CORBA.Object.Repository_Id, CORBA.Object.Nil_Ref, null);
   --  Register CORBA.Object root interface. Work around for
   --  elaboration circularity.

end CORBA.Object.OmniORB;
