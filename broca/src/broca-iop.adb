------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O P                             --
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

with Ada.Unchecked_Deallocation;
with GNAT.HTable;

with CORBA;             use CORBA;
with Broca.Exceptions;
with Broca.Buffers;     use Broca.Buffers;
with Broca.CDR;         use Broca.CDR;

with Broca.Debug;
pragma Elaborate (Broca.Debug);

package body Broca.IOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iop");
   procedure O is new Broca.Debug.Output (Flag);

   function Unmarshall_Callback
     (Tag : Profile_Tag)
     return Unmarshall_Profile_Body_Type;
   --  Return the registered unmarshalling subprogram
   --  for the given Tag.

   -----------------------------------
   -- Abstract GIOP connection type --
   -----------------------------------

   function Get_Request
     (Connection : access Connection_Type)
     return CORBA.Unsigned_Long
   is
      Result : CORBA.Unsigned_Long;

   begin
      Result := Connection.Request;
      Connection.Request := Connection.Request + 1;
      return Result;
   end Get_Request;

   --  A place-holder profile type for unknown profile tags.

   type Encapsulation_Ptr is access Broca.Buffers.Encapsulation;

   procedure Free is new Ada.Unchecked_Deallocation
     (Broca.Buffers.Encapsulation, Encapsulation_Ptr);

   type Unknown_Profile_Type is new Profile_Type with record
      Tag  : Profile_Tag;
      Data : Encapsulation_Ptr
        := null;
   end record;

   function Get_Object_Key
     (Profile : Unknown_Profile_Type)
     return Broca.Sequences.Octet_Sequence;

   function Find_Connection
     (Profile : access Unknown_Profile_Type)
     return Connection_Ptr;

   function Get_Profile_Tag
     (Profile : Unknown_Profile_Type)
     return Profile_Tag;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type);

   procedure Finalize
     (X : in out Unknown_Profile_Type);

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

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type) is
   begin
      Marshall (Buffer, Profile.Data);
   end Marshall_Profile_Body;

   procedure Finalize
     (X : in out Unknown_Profile_Type) is
   begin
      Free (X.Data);
   end Finalize;

   --------------------------------
   -- Abstract GIOP profile type --
   --------------------------------

   procedure Marshall_Tagged_Profile
     (Buffer : access Buffer_Type;
      Profile : Profile_Type'Class) is
   begin
      Marshall (Buffer, Get_Profile_Tag (Profile));
      Marshall_Profile_Body (Buffer, Profile);
   end Marshall_Tagged_Profile;

   function Unmarshall_Tagged_Profile
     (Buffer : access Buffer_Type)
     return Profile_Ptr
   is
      Tag : constant Profile_Tag
        := Unmarshall (Buffer);

      Unmarshall_Profile_Body : constant Unmarshall_Profile_Body_Type
        := Unmarshall_Callback (Tag);

   begin
      if Unmarshall_Profile_Body /= null then
         return Unmarshall_Profile_Body (Buffer);
      end if;

      declare
         Res_Ptr : constant Profile_Ptr
           := new Unknown_Profile_Type;
         Res : Unknown_Profile_Type
           renames Unknown_Profile_Type (Res_Ptr.all);
      begin
         Res.Tag  := Tag;
         Res.Data := new Encapsulation'(Unmarshall (Buffer));

         return Res_Ptr;
      end;

   end Unmarshall_Tagged_Profile;

   -----------------------
   -- Object References --
   -----------------------

   procedure Decapsulate_IOR
     (Buffer   : access Buffer_Type;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr)
   is
      N_Profiles : CORBA.Unsigned_Long;
   begin
      Type_Id := Unmarshall (Buffer);
      N_Profiles := Unmarshall (Buffer);
      pragma Debug (O ("Decapsulate_IOR: type "
                       & To_Standard_String (Type_Id)
                       & " (" & N_Profiles'Img & " profiles)."));

      Profiles := new Profile_Ptr_Array'(1 .. N_Profiles => null);

      for N in Profiles'Range loop
         Profiles (N) := Unmarshall_Tagged_Profile (Buffer);
      end loop;
   end Decapsulate_IOR;

   procedure Encapsulate_IOR
     (Buffer   : access Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr) is
   begin

      Marshall (Buffer, Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profiles'Length));

      for N in Profiles'Range loop
         Marshall_Tagged_Profile (Buffer, Profiles (N).all);
      end loop;
   end Encapsulate_IOR;

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
     return Hash_Type is
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

end Broca.IOP;
