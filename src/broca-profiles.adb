with Ada.Unchecked_Deallocation;

with Broca.Sequences;
with Broca.CDR; use Broca.CDR;
with Broca.Exceptions;
with Broca.Debug;

with GNAT.HTable;

package body Broca.Profiles is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.profiles");
   procedure O is new Broca.Debug.Output (Flag);

   function Unmarshall_Callback
     (Tag : Profile_Tag)
     return Unmarshall_Profile_Body_Type;
   --  Return the registered unmarshalling subprogram
   --  for the given Tag.

   --------------
   -- Register --
   --------------

   type Hash_Type is range 0 .. 26;
   Hash_Mod : constant := Hash_Type'Last + 1;

   function Hash_Profile_Tag
     (Tag : Profile_Tag)
     return Hash_Type;

   function Hash_Profile_Tag
     (Tag : Profile_Tag)
     return Hash_Type
   is
      use CORBA;
   begin
      return Hash_Type (Tag mod Hash_Mod);
   end Hash_Profile_Tag;

   function Equals_Profile_Tag
     (T1 : Profile_Tag;
      T2 : Profile_Tag)
     return Boolean
     renames CORBA."=";

   package IOP_HT is
      new GNAT.HTable.Simple_HTable
     (Hash_Type,
      Unmarshall_Profile_Body_Type,
      null,
      Profile_Tag,
      Hash_Profile_Tag,
      Equals_Profile_Tag);

   procedure Register
     (Tag     : in Profile_Tag;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type) is
   begin
      pragma Debug (O ("Registering callback for tag" & Tag'Img));
      IOP_HT.Set (Tag, Unmarshall_Profile_Body);
   end Register;

   function Unmarshall_Callback
     (Tag : Profile_Tag)
     return Unmarshall_Profile_Body_Type is
   begin
      pragma Debug (O ("Retrieving callback for tag" & Tag'Img));
      return IOP_HT.Get (Tag);
   end Unmarshall_Callback;

   ------------------------
   --  Unknwon_Profile_Type
   ------------------------

   function Get_Object_Key
     (Profile : Unknown_Profile_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      Broca.Exceptions.Raise_Bad_Param;
      return Broca.Sequences.Null_Sequence;
   end Get_Object_Key;

   function Find_Connection
     (Profile : access Unknown_Profile_Type)
     return Connection_Ptr is
   begin
      Broca.Exceptions.Raise_Bad_Param;
      return null;
   end Find_Connection;

   function Get_Profile_Tag
     (Profile : Unknown_Profile_Type)
     return Profile_Tag is
   begin
      return Profile.Tag;
   end Get_Profile_Tag;

   function Get_Profile_Priority
     (Profile : in Unknown_Profile_Type)
     return Profile_Priority is
   begin
      return Profile_Priority'First;
   end Get_Profile_Priority;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type) is
   begin
      Marshall (Buffer, Profile.Data.all);
   end Marshall_Profile_Body;

--     function Unmarshall_Profile_Body
--       (Buffer  : access Buffers.Buffer_Type)
--        return Unknown_Profile_Access
--     is
--        Profile : Unknown_Profile_Access := new Unknown_Profile_type;
--     begin
--        Profile.Data := Unmarshall (Buffer);
--        return Profile;!!
--     end Unmarshall_Profile_Body;


   procedure Finalize
     (X : in out Unknown_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Broca.Buffers.Encapsulation, Encapsulation_Ptr);
   begin
      Free (X.Data);
   end Finalize;

   ----------------------------
   --  Tagged component profile
   ----------------------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Tagged_Component : Tagged_Component_Type)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Tagged_Component.Tag));
      Marshall (Buffer, Tagged_Component.Component_Data);
   end Marshall;

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
      return Tagged_Component_Access
   is
      Tag : CORBA.Unsigned_Long := Unmarshall (Buffer);
      Data : Octet_Array := Unmarshall (Buffer);
   begin
      return new Tagged_Component_Type'
        (Encapsulation_Size => Data'Length,
         Tag => Component_Id (Tag),
         Component_Data => Data);
   end Unmarshall;

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Components : in Tagged_Component_Array)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Components'Length));
      for I in Components'Range loop
         Marshall (Buffer, Components (I).all);
      end loop;
   end Marshall;

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Tagged_Components_Ptr
   is
      use CORBA;
      Length : CORBA.Unsigned_Long;
      Result : Tagged_Components_Ptr;
   begin
      Length := Unmarshall (Buffer);
      Result := new Tagged_Component_Array (0 .. Length - 1);
      for I in Result'Range loop
         Result (I) := Unmarshall (Buffer);
      end loop;
      return Result;
   end Unmarshall;

   procedure Finalization
     (Profile : in out Multiple_Component_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component_Type, Tagged_Component_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component_Array, Tagged_Components_Ptr);
   begin
      for I in Profile.Components'Range loop
         Free (Profile.Components (I));
      end loop;
      Free (Profile.Components);
   end Finalization;

   function Get_Object_Key
     (Profile : Multiple_Component_Profile_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      Broca.Exceptions.Raise_Internal;
      return Broca.Sequences.Null_Sequence;
   end Get_Object_Key;

   function Find_Connection
     (Profile : access Multiple_Component_Profile_Type)
     return Connection_Ptr is
   begin
      Broca.Exceptions.Raise_Internal;
      return null;
   end Find_Connection;

   function Get_Profile_Tag
     (Profile : Multiple_Component_Profile_Type)
      return Profile_Tag is
   begin
      return Tag_Multiple_Components;
   end Get_Profile_Tag;

   function Get_Profile_Priority
     (Profile : in Multiple_Component_Profile_Type)
     return Profile_Priority is
   begin
      return Profile_Priority'First;
   end Get_Profile_Priority;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Multiple_Component_Profile_Type) is
   begin
      Marshall (Buffer, Profile.Components.all);
   end Marshall_Profile_Body;

   --------------------------------
   -- Abstract GIOP profile type --
   --------------------------------

   procedure Marshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type;
      Profile : Profile_Type'Class) is
   begin
      Marshall (Buffer, Get_Profile_Tag (Profile));
      Marshall_Profile_Body (Buffer, Profile);
   end Marshall_Tagged_Profile;

   function Unmarshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type)
     return Profile_Ptr
   is
      Tag : constant Profile_Tag
        := Unmarshall (Buffer);

      Unmarshall_Profile_Body : constant Unmarshall_Profile_Body_Type
        := Unmarshall_Callback (Tag);

   begin
      pragma Debug (O ("Unmarshall_Tagged_Profile : enter"));
      if Unmarshall_Profile_Body /= null then
         pragma Debug (O ("Unmarshall_Tagged_Profile : "
                          & "Unmarshall_Profile_Body /= null"));
         return Unmarshall_Profile_Body (Buffer);
      end if;

      declare
         Res_Ptr : constant Profile_Ptr
           := new Unknown_Profile_Type;
         Res : Unknown_Profile_Type
           renames Unknown_Profile_Type (Res_Ptr.all);
      begin
         Res.Tag  := Tag;
         Res.Data := new Buffers.Encapsulation'(Unmarshall (Buffer));

         pragma Debug (O ("Unmarshall_Tagged_Profile : "
                          & "Unmarshall_Profile_Body = null"));
         return Res_Ptr;
      end;

   end Unmarshall_Tagged_Profile;

   -----------------------
   -- Object References --
   -----------------------

   procedure Decapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr;
      Used_Profile_Index : out CORBA.Unsigned_Long;
      Is_Supported_Profile : out Boolean)
   is
      N_Profiles : CORBA.Unsigned_Long;
   begin
      Type_Id := Unmarshall (Buffer);
      N_Profiles := Unmarshall (Buffer);
      pragma Debug (O ("Decapsulate_IOR: type "
                       & CORBA.To_Standard_String (Type_Id)
                       & " (" & N_Profiles'Img & " profiles)."));

      Profiles := new Profile_Ptr_Array'(1 .. N_Profiles => null);

      for N in Profiles'Range loop
         Profiles (N) := Unmarshall_Tagged_Profile (Buffer);
      end loop;

      Find_Best_Profile
        (Profiles, Used_Profile_Index, Is_Supported_Profile);

   end Decapsulate_IOR;

   procedure Encapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr) is
   begin

      Marshall (Buffer, Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profiles'Length));

      for N in Profiles'Range loop
         Marshall_Tagged_Profile (Buffer, Profiles (N).all);
      end loop;
   end Encapsulate_IOR;

end Broca.Profiles;
