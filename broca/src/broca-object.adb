------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . O B J E C T                          --
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

with Ada.Unchecked_Deallocation;

with Broca.IOP;
with Broca.Profiles; use Broca.Profiles;
with Broca.Buffers; use Broca.Buffers;
with Broca.Exceptions;
with Broca.Debug;

package body Broca.Object is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.object");
   procedure O is new Broca.Debug.Output (Flag);

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Object_Type)
   is
      use Broca.IOP;

      procedure Free is
         new Ada.Unchecked_Deallocation
        (Profile_Type'Class, Profile_Ptr);
      procedure Free is
         new Ada.Unchecked_Deallocation
        (Profile_Ptr_Array, Profile_Ptr_Array_Ptr);

   begin
      pragma Debug (O ("In Finalize (Object_Type):"));
      if X.Profiles /= null then
         for I in X.Profiles'Range loop
            pragma Debug (O ("Freeing Profiles (" & I'Img & ")"));
            Free (X.Profiles (I));
         end loop;
         pragma Debug (O ("Freeing Profiles"));
         Free (X.Profiles);
      end if;
   end Finalize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Broca.Object.Object_Type)
   is
      use Broca.IOP;
   begin
      if Value.Profiles = null then
         Broca.Exceptions.Raise_Internal;
      end if;
      Broca.Profiles.Encapsulate_IOR
        (Buffer, Value.Type_Id, Value.Profiles);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Result : out Broca.Object.Object_Type)
   is
   begin
      Broca.Profiles.Decapsulate_IOR
        (Buffer,
         Result.Type_Id,
         Result.Profiles,
         Result.Used_Profile_Index,
         Result.Is_Supported_Profile
         );
   end Unmarshall;

   ------------------
   -- Find_Profile --
   ------------------

   function Find_Profile
     (Object : Object_Ptr)
     return IOP.Profile_Ptr
   is
      use CORBA;
      use Broca.IOP;

   begin
      pragma Assert (Object /= null);
      if Object.Local_Object then
         --  Should not be called for local objects
         Broca.Exceptions.Raise_Internal;
      elsif Object.Profiles /= null then
         return Object.Profiles (Object.Used_Profile_Index);
      else
         Broca.Exceptions.Raise_Internal;
      end if;
   end Find_Profile;

   ---------------
   --  Get_Type_Id
   ---------------

   function Get_Type_Id (Object : in Object_Type) return CORBA.String is
   begin
      return Object.Type_Id;
   end Get_Type_Id;

   ---------------------------
   --  Create_Object_From_IOR
   ---------------------------

   function Create_Object_From_IOR
     (IOR : access Broca.Buffers.Buffer_Type)
     return Object_Ptr
   is
      Type_Id : CORBA.String;
      Obj : Object_Ptr := new Object_Type (Local_Object => False);
   begin
      pragma Debug (O ("Create_Object_From_IOR : enter"));

      Broca.Profiles.Decapsulate_IOR
        (IOR,
         Obj.Type_Id,
         Obj.Profiles,
         Obj.Used_Profile_Index,
         Obj.Is_Supported_Profile
         );

      pragma Debug (O ("Create_Object_From_IOR : Type_Id unmarshalled : "
                       & CORBA.To_Standard_String (Type_Id)));
      return Obj;
   end Create_Object_From_IOR;

   ----------------------
   --  Create custom IOR
   ----------------------

--     function Create_Custom_Object
--       (Type_Id : in CORBA.String;
--        Host : in CORBA.String;
--        Port : in CORBA.Unsigned_Short;
--        Object_Key : in Broca.Sequences.Octet_Sequence)
--        return Object_Ptr
--     is
--        use Broca.IIOP;
--        Profile : Profile_IIOP_1_0_Access := new Profile_IIOP_1_0_Type;
--        Obj : Object_Ptr := new Object_Type;
--     begin
--        Profile.Host := Host;
--        Profile.Port := Port;
--        Profile.ObjKey := Object_Key;
--        Obj.Type_Id := Type_Id;
--        Obj.Profiles := new Broca.IOP.Profile_Ptr_Array'
--          (1 => Broca.IOP.Profile_Ptr (Profile));
--        Obj.Used_Profile_Index := 1;
--        Obj.Is_Supported_Profile := True;
--        return Obj;
--     end Create_Custom_Object;

   function Create_Object
     (Type_Id : in CORBA.String;
      Profiles : Broca.IOP.Profile_Ptr_Array_Ptr;
      Local_Object : in Boolean)
     return Object_Ptr
   is
      Obj : Object_Ptr := new Object_Type (Local_Object);
   begin
      Obj.Type_Id := Type_Id;
      Obj.Profiles := Profiles;
      if not Local_Object then
         Broca.IOP.Find_Best_Profile
           (Profiles, Obj.Used_Profile_Index, Obj.Is_Supported_Profile);
      end if;
      return Obj;
   end Create_Object;

end Broca.Object;




