------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O P                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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

with Broca.Buffers;     use Broca.Buffers;
with Broca.Marshalling; use Broca.Marshalling;
with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

with CORBA;             use CORBA;

package body Broca.IOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iop");
   procedure O is new Broca.Debug.Output (Flag);

   type Profile_Record is
      record
         Encapsulate : Encapsulate_Profile_Type;
         Decapsulate : Decapsulate_Profile_Type;
      end record;

   Callbacks : array (Tag_Internet_IOP .. Tag_Multiple_Components)
     of Profile_Record;

   ---------------------
   -- Decapsulate_IOR --
   ---------------------

   procedure Decapsulate_IOR
     (Buffer   : in out Buffers.Buffer_Descriptor;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr)
   is
      Old_Endian : Boolean := Get_Endianess (Buffer);
      N_Profiles : CORBA.Unsigned_Long;
      Profile_Length : CORBA.Unsigned_Long;
      Profile    : Profile_Id;
   begin
      Unmarshall    (Buffer, Type_Id);
      Unmarshall    (Buffer, N_Profiles);

      Profiles := new Profile_Ptr_Array (1 .. N_Profiles);

      for N in Profiles'Range loop
         Unmarshall  (Buffer, Profile);
         Unmarshall (Buffer, Profile_Length);
         Callbacks (Profile).Decapsulate (Buffer, Profiles (N));
      end loop;
      Set_Endianess (Buffer, Old_Endian);
   end Decapsulate_IOR;

   ---------------------
   -- Encapsulate_IOR --
   ---------------------

   procedure Encapsulate_IOR
     (Buffer   : in out Buffers.Buffer_Descriptor;
      From     : in Buffers.Buffer_Index_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr)
   is
      Buffers  : array (Profiles'Range) of Buffer_Descriptor;
      Offsets  : array (Profiles'Range) of Buffer_Index_Type;
      Profile_Lengths : array (Profiles'Range) of CORBA.Long;
   begin
      Rewind              (Buffer);
      Skip_Bytes          (Buffer, From);
      Compute_New_Size    (Buffer, Type_Id);

      --  Number of profiles
      Compute_New_Size    (Buffer, UL_Size, UL_Size);

      for N in Profiles'Range loop
         --  Tag of profile
         Compute_New_Size    (Buffer, UL_Size, UL_Size);

         --  Length of profile
         Compute_New_Size    (Buffer, UL_Size, UL_Size);

         Offsets (N) := Size_Used (Buffer);
         Callbacks (Get_Profile_Id (Profiles (N).all)).Encapsulate
           (Buffers (N), Offsets (N), Profiles (N));

         --  Skip space for profile
         Profile_Lengths (N) :=
           CORBA.Long (Full_Size (Buffers (N)) - Offsets (N));

         Skip_Bytes (Buffer, Buffer_Index_Type (Profile_Lengths (N)));
      end loop;

      Allocate_Buffer_And_Clear_Pos (Buffer, Full_Size (Buffer));

      Skip_Bytes          (Buffer, From);

      Marshall (Buffer, Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profiles'Length));

      for N in Profiles'Range loop
         Rewind        (Buffers (N));
         Skip_Bytes    (Buffers (N), Offsets (N));
         pragma Debug (O ("Dump Buffers (N)"));
         pragma Debug (Show (Buffers (N)));
         Marshall      (Buffer, Get_Profile_Id (Profiles (N).all));

         Marshall  (Buffer, Profile_Lengths (N));

         Skip_Bytes    (Buffer, Offsets (N) - Size_Used (Buffer));
         Append_Buffer (Buffer, Buffers (N));
         Destroy       (Buffers (N));
      end loop;
   end Encapsulate_IOR;

   --------------
   -- Register --
   --------------

   procedure Register
     (Profile     : in Profile_Id;
      Encapsulate : in Encapsulate_Profile_Type;
      Decapsulate : in Decapsulate_Profile_Type) is
   begin
      Callbacks (Profile).Encapsulate := Encapsulate;
      Callbacks (Profile).Decapsulate := Decapsulate;
   end Register;

end Broca.IOP;
