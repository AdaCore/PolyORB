------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . O R B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with Ada.Exceptions;

with CORBA.Sequences.Unbounded;
with CORBA.Impl;

with Broca.Buffers;
with Broca.CDR;
with Broca.IOP;
with Broca.Environment;
with Broca.Exceptions;
with Broca.Names;
with Broca.Object;
with Broca.Repository;
with Broca.Sequences;

with Broca.Debug;
pragma Elaborate (Broca.Debug);

with Broca.IIOP;
pragma Warnings (Off, Broca.IIOP);

with Broca.POA;
with PortableServer.POA;
with PortableServer.ServantLocator.Impl;
with PortableServer.ServantManager;

package body Broca.ORB is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.orb");
   procedure O is new Broca.Debug.Output (Flag);

   use CORBA.ORB;

   The_ORB : ORB_Ptr := null;

   package IDL_SEQUENCE_Ref is
     new CORBA.Sequences.Unbounded (CORBA.Object.Ref);

   Initial_References_POA_Name : constant CORBA.String :=
     CORBA.To_CORBA_String ("InitialReferences");

   Identifiers : ObjectIdList;
   References  : IDL_SEQUENCE_Ref.Sequence;

   References_POA         : Broca.POA.POA_Object_Ptr;

   type Profile_IIOP_Ptr is access Broca.IIOP.Profile_IIOP_Type;

   type References_Locator is
     new PortableServer.ServantLocator.Impl.Object with null record;

   procedure Preinvoke
     (Self       : in out References_Locator;
      Oid        : in PortableServer.ObjectId;
      Adapter    : in PortableServer.POA.Ref;
      Operation  : in CORBA.Identifier;
      The_Cookie : out PortableServer.ServantLocator.Cookie;
      Returns    : out PortableServer.Servant);

   procedure Postinvoke
     (Self        : in out References_Locator;
      Oid         : in PortableServer.ObjectId;
      Adapter     : in PortableServer.POA.Ref;
      Operation   : in CORBA.Identifier;
      The_Cookie  : in PortableServer.ServantLocator.Cookie;
      The_Servant : in PortableServer.Servant);

   Locator : aliased References_Locator;

   procedure Ensure_References_POA_Is_Started;

   function To_String (Oid : PortableServer.ObjectId)
     return String;

   function To_Octet_Sequence (Str : CORBA.String)
     return Broca.Sequences.Octet_Sequence;

   function Build_Remote_Naming_Reference return CORBA.Object.Ref;

   function Build_Object_Key
     (POA_Name    : CORBA.String;
      Object_Name : CORBA.String)
     return Broca.Sequences.Octet_Sequence;
   --  Build an object key for a persistent POA and a fixed object name.

   ----------------------
   -- Build_Object_Key --
   ----------------------

   function Build_Object_Key
     (POA_Name    : CORBA.String;
      Object_Name : CORBA.String)
     return Broca.Sequences.Octet_Sequence
   is
      use Broca.Buffers, Broca.CDR;
      Buffer     : aliased Buffer_Type;
      Key_Buffer : aliased Buffer_Type;
   begin
      Start_Encapsulation (Buffer'Access);

      --  Boot time: 0 for a persistent POA
      Marshall (Buffer'Access, CORBA.Unsigned_Long'(0));

      --  POA index (useless)
      Marshall (Buffer'Access, CORBA.Unsigned_Long'(0));

      --  Date of the entry (useless)
      Marshall (Buffer'Access, CORBA.Unsigned_Long'(0));

      --  Number of POAs in the POA path name: 1
      Marshall (Buffer'Access, CORBA.Unsigned_Long'(1));

      --  Name of the POA
      Marshall (Buffer'Access, POA_Name);

      --  Object key for the POA
      Start_Encapsulation (Key_Buffer'Access);
      Broca.Sequences.Marshall
        (Key_Buffer'Access, To_Octet_Sequence (Object_Name));
      Broca.Buffers.Show (Key_Buffer);
      Marshall (Buffer'Access, Encapsulate (Key_Buffer'Access));

      return
        Broca.Sequences.Octet_Sequences.To_Sequence
        (Broca.Sequences.To_CORBA_Octet_Array (Encapsulate (Buffer'Access)));
   end Build_Object_Key;

   -----------------------------------
   -- Build_Remote_Naming_Reference --
   -----------------------------------

   function Build_Remote_Naming_Reference return CORBA.Object.Ref is
      use Broca.Environment;
      Host : constant String   := Get_Conf (Naming_Host, Naming_Host_Default);
      Port : constant Positive :=
        Positive'Value (Get_Conf (Naming_Port, Naming_Port_Default));
      Profile : Profile_IIOP_Ptr := new Broca.IIOP.Profile_IIOP_Type;
      Obj     : Broca.Object.Object_Ptr := new Broca.Object.Object_Type;
      Result  : CORBA.Object.Ref;
   begin
      Obj.Type_Id             :=
        CORBA.To_CORBA_String
        (Broca.Names.OMG_RepositoryId ("CosNaming/NamingContext"));
      Profile.Host   := CORBA.To_CORBA_String (Host);
      Profile.Port   := CORBA.Unsigned_Short (Port);
      Profile.ObjKey :=
        Build_Object_Key
        (Initial_References_POA_Name,
         CORBA.String (Name_Service_ObjectId));
      Obj.Profiles :=
        new Broca.IOP.Profile_Ptr_Array'(1 =>
                                           Broca.IOP.Profile_Ptr (Profile));
      CORBA.Object.Set (Result, CORBA.Impl.Object_Ptr (Obj));
      return Result;
   end Build_Remote_Naming_Reference;

   --------------------------------------
   -- Ensure_References_POA_Is_Started --
   --------------------------------------

   procedure Ensure_References_POA_Is_Started is
      use Broca.POA, PortableServer;
   begin
      pragma Debug (O ("Ensuring that references POA is started"));
      if References_POA = null then

         pragma Debug (O ("It has not been started"));

         declare
            RootPOA_Obj : constant CORBA.Object.Ref'Class :=
              Resolve_Initial_References (Root_POA_ObjectId);
            RootPOA_Ptr : constant Broca.POA.POA_Object_Ptr :=
              Broca.POA.POA_Object_Ptr
              (CORBA.Object.Object_Of (RootPOA_Obj));
         begin
            pragma Debug (O ("Starting references POA"));
            References_POA :=
              Broca.POA.Create_POA
              (Self         => RootPOA_Ptr,
               Adapter_Name => Initial_References_POA_Name,
               A_POAManager => null,
               Tp           => ORB_CTRL_MODEL,
               Lp           => PERSISTENT,
               Up           => UNIQUE_ID,
               Ip           => USER_ID,
               Ap           => NO_IMPLICIT_ACTIVATION,
               Sp           => NON_RETAIN,
               Rp           => USE_SERVANT_MANAGER);
            pragma Debug (O ("Setting default servant"));
            PortableServer.ServantManager.Set
              (References_POA.Servant_Manager,
               CORBA.Impl.Object_Ptr'(Locator'Access));
            pragma Debug (O ("Activating the references POA"));
            Activate (Get_The_POAManager (References_POA) .all);
            pragma Debug (O ("References POA ready"));
         end;
      end if;
   exception
      when E : others =>
         pragma Debug (O ("Got an exception: " &
                          Ada.Exceptions.Exception_Information (E)));
         raise;
   end Ensure_References_POA_Is_Started;

   -------------------
   -- IOR_To_Object --
   -------------------

   procedure IOR_To_Object
     (IOR : access Broca.Buffers.Buffer_Type;
      Ref : out CORBA.Object.Ref'Class)
   is
      Type_Id : CORBA.String;
      Profiles : Broca.IOP.Profile_Ptr_Array_Ptr;

   begin
      pragma Debug (O ("IOR_To_Object : enter"));

      Broca.IOP.Decapsulate_IOR (IOR, Type_Id, Profiles);

      pragma Debug (O ("IOR_To_Object : Type_Id unmarshalled : "
                       & CORBA.To_Standard_String (Type_Id)));

      declare
         A_Ref : CORBA.Object.Ref'Class :=
           Broca.Repository.Create (CORBA.RepositoryId (Type_Id));
      begin
         if CORBA.Object.Is_Nil (A_Ref) then
            --  No classes for the string was found.
            --  Create a CORBA.Object.Ref.
            pragma Debug (O ("IOR_To_Object : A_Ref is nil."));
            --  Broca.Refs.Set (Broca.Refs.Ref (A_Ref),
            --                new Broca.Object.Object_Type);
            CORBA.Object.Set
              (A_Ref,
               CORBA.Impl.Object_Ptr'(new Broca.Object.Object_Type));
         end if;

         pragma Assert (not CORBA.Object.Is_Nil (A_Ref));

         declare
            --  Get the access to the internal object.
            Obj : constant Broca.Object.Object_Ptr
              := Broca.Object.Object_Ptr
              (CORBA.Object.Object_Of (A_Ref));
         begin
            Obj.Type_Id  := Type_Id;
            Obj.Profiles := Profiles;
         end;

         CORBA.Object.Set (Ref, CORBA.Object.Object_Of (A_Ref));

         --  We want to write
         --    Ref := A_Ref;
         --  but, since Ref is an out classwide parameter,
         --  its tag is constrained by the value passed
         --  to us by the caller. If that value is of a
         --  specialised reference type, i. e. any particular
         --  reference type generated in an interface stub
         --  package, we cannot assign A_Ref into it because
         --  A_Ref may be created as a CORBA.Object.Ref if
         --  no factory is registered for the unmarshalled
         --  repository ID.
         --
         --  We thus have to trust the user to pass us a proper
         --  reference type, and bypass any dynamic tag check
         --  on the reference type.

      end;

   end IOR_To_Object;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services return ObjectIdList is
   begin
      return Identifiers;
   end List_Initial_Services;

   ---------------
   -- Preinvoke --
   ---------------

   procedure Preinvoke
     (Self       : in out References_Locator;
      Oid        : in PortableServer.ObjectId;
      Adapter    : in PortableServer.POA.Ref;
      Operation  : in CORBA.Identifier;
      The_Cookie : out PortableServer.ServantLocator.Cookie;
      Returns    : out PortableServer.Servant)
   is
      Name : constant String := To_String (Oid);
      Obj  : constant CORBA.Object.Ref'Class :=
        Resolve_Initial_References (CORBA.ORB.To_CORBA_String (Name));
   begin
      pragma Debug (O ("In Preinvoke for service " & Name &
                       " and operation " &
                       CORBA.To_Standard_String (Operation)));
      The_Cookie := null;
      Returns    :=
        PortableServer.POA.Reference_To_Servant
        (PortableServer.POA.Get_The_Parent (Adapter), Obj);
   end Preinvoke;

   ----------------
   -- Postinvoke --
   ----------------

   procedure Postinvoke
     (Self        : in out References_Locator;
      Oid         : in PortableServer.ObjectId;
      Adapter     : in PortableServer.POA.Ref;
      Operation   : in CORBA.Identifier;
      The_Cookie  : in PortableServer.ServantLocator.Cookie;
      The_Servant : in PortableServer.Servant)
   is
   begin
      pragma Debug (O ("Calling Postinvoke for service " & To_String (Oid) &
                       " and operation " &
                       CORBA.To_Standard_String (Operation)));
      null;
   end Postinvoke;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref'Class
   is
      use CORBA.ORB.IDL_SEQUENCE_ObjectId;
      use IDL_SEQUENCE_Ref;
   begin
      for I in 1 .. Length (Identifiers) loop
         if Element_Of (Identifiers, I) = Identifier then
            pragma Debug (O ("Resolved initial reference " &
                             To_Standard_String (Identifier)));
            return Element_Of (References, I);
         end if;
      end loop;
      if To_Standard_String (Identifier) = "NamingService" then
         pragma Debug (O ("Returning remote reference of NamingService"));
         declare
            Ref : constant CORBA.Object.Ref'Class :=
              Build_Remote_Naming_Reference;
         begin
            --  Register the computed remote naming service reference

            Register_Initial_Reference (Identifier, Ref);
            return Ref;
         end;
      end if;
      pragma Debug (O ("Initial reference of " &
                       To_Standard_String (Identifier) & " is unknown, " &
                       "raising InvalidName"));
      raise CORBA.InvalidName;
   end Resolve_Initial_References;

   --------------------------------
   -- Register_Initial_Reference --
   --------------------------------

   procedure Register_Initial_Reference
     (Identifier : in CORBA.ORB.ObjectId;
      Reference  : in CORBA.Object.Ref'Class)
   is
      use CORBA.ORB.IDL_SEQUENCE_ObjectId;
      use IDL_SEQUENCE_Ref;
   begin
      --  If we register initial references other than the RootPOA,
      --  then it is likely that we are willing to export those services.

      if Identifier /= Root_POA_ObjectId then
         Ensure_References_POA_Is_Started;
      end if;

      Append (Identifiers, Identifier);
      Append (References, CORBA.Object.Ref (Reference));
   end Register_Initial_Reference;

   ------------------
   -- Register_ORB --
   ------------------

   procedure Register_ORB (ORB : ORB_Ptr) is
   begin
      if The_ORB /= null then
         --  Only one ORB can register.
         Broca.Exceptions.Raise_Internal (1000, CORBA.Completed_No);
      end if;
      The_ORB := ORB;
   end Register_ORB;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      if The_ORB = null then
         return;
      else
         Run (The_ORB.all);
      end if;
   end Run;

   -----------------------
   -- POA_State_Changed --
   -----------------------

   procedure POA_State_Changed
     (POA : in Broca.POA.Ref) is
   begin
      POA_State_Changed (The_ORB.all, POA);
   end POA_State_Changed;

   -----------------------
   -- To_Octet_Sequence --
   -----------------------

   function To_Octet_Sequence (Str : CORBA.String)
     return Broca.Sequences.Octet_Sequence
   is
      Ada_Str : constant String := CORBA.To_Standard_String (Str);
      Value   : Broca.Sequences.CORBA_Octet_Array (1 .. CORBA.Length (Str));
      Index   : Positive := 1;
   begin
      for I in Ada_Str'Range loop
         Value (I) := CORBA.Octet'Val (Character'Pos (Ada_Str (I)));
         Index := Index + 1;
      end loop;
      return Broca.Sequences.Octet_Sequences.To_Sequence (Value);
   end To_Octet_Sequence;

   ---------------
   -- To_String --
   ---------------

   function To_String (Oid : PortableServer.ObjectId) return String
   is
      Result : String (1 .. PortableServer.Length (Oid));
      Chars  : constant PortableServer.IDL_SEQUENCE_Octet.Element_Array :=
        PortableServer.To_Element_Array (Oid);
      Index  : Positive := 1;
   begin
      for I in Chars'Range loop
         Result (Index) := Character'Val (Chars (I));
         Index := Index + 1;
      end loop;
      return Result;
   end To_String;

end Broca.ORB;
