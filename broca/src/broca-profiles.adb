------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       B R O C A . P R O F I L E S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with Broca.CDR; use Broca.CDR;
with Broca.Debug;

with Broca.Profiles.Unknown;      use Broca.Profiles.Unknown;

with GNAT.HTable;

package body Broca.Profiles is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.profiles");
   procedure O is new Broca.Debug.Output (Flag);

   function Unmarshall_Callback
     (Tag : Profile_Tag)
     return Unmarshall_Profile_Body_Type;
   --  Return the registered unmarshalling subprogram
   --  for the given Tag.

   type Hash_Type is range 0 .. 26;
   Hash_Mod : constant := Hash_Type'Last + 1;

   function Hash_Profile_Tag
     (Tag : Profile_Tag)
     return Hash_Type;

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

   ---------------------
   -- Decapsulate_IOR --
   ---------------------

   procedure Decapsulate_IOR
     (Buffer               : access Buffers.Buffer_Type;
      Type_Id              : out CORBA.String;
      Profiles             : out Profile_Ptr_Array_Ptr;
      Used_Profile_Index   : out CORBA.Unsigned_Long;
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

   ---------------------
   -- Encapsulate_IOR --
   ---------------------

   procedure Encapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr)
   is
   begin
      Marshall (Buffer, Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profiles'Length));

      for N in Profiles'Range loop
         Marshall_Tagged_Profile (Buffer, Profiles (N).all);
      end loop;
   end Encapsulate_IOR;

   ----------------------
   -- Hash_Profile_Tag --
   ----------------------

   function Hash_Profile_Tag
     (Tag : Profile_Tag)
     return Hash_Type
   is
      use CORBA;
   begin
      return Hash_Type (Tag mod Hash_Mod);
   end Hash_Profile_Tag;

   -----------------------------
   -- Marshall_Tagged_Profile --
   -----------------------------

   procedure Marshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type;
      Profile : Profile_Type'Class)
   is
   begin
      Marshall (Buffer, Get_Profile_Tag (Profile));
      Marshall_Profile_Body (Buffer, Profile);
   end Marshall_Tagged_Profile;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag     : in Profile_Tag;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type)
   is
   begin
      pragma Debug (O ("Registering callback for tag" & Tag'Img));
      IOP_HT.Set (Tag, Unmarshall_Profile_Body);
   end Register;

   -------------------------
   -- Unmarshall_Callback --
   -------------------------

   function Unmarshall_Callback
     (Tag : Profile_Tag)
     return Unmarshall_Profile_Body_Type is
   begin
      pragma Debug (O ("Retrieving callback for tag" & Tag'Img));
      return IOP_HT.Get (Tag);
   end Unmarshall_Callback;

   -------------------------------
   -- Unmarshall_Tagged_Profile --
   -------------------------------

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

end Broca.Profiles;
