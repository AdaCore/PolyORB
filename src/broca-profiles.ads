------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       B R O C A . P R O F I L E S                        --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;

with Broca.Buffers;
with Broca.IOP;      use Broca.IOP;

package Broca.Profiles is

   procedure Register
     (Tag                     : in Profile_Tag;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type);
   --  Register Unmarshall_Profile_Body as the function used
   --  to unmarshall a Tagged Profile Body corresponding to Tag.

   procedure Marshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type;
      Profile : Profile_Type'Class);
   --  Marshall a TaggedProfile into Buffer.

   function Unmarshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type)
     return Profile_Ptr;
   --  Unmarshall a TaggedProfile from Buffer.
   --  The Profile_Type designated by the returned
   --  Profile_Ptr is dynamically allocated; it is up
   --  to the caller to release the associated storage
   --  when the profile is not needed anymore.

   procedure Encapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr);

   procedure Decapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr;
      Used_Profile_Index : out CORBA.Unsigned_Long;
      Is_Supported_Profile : out Boolean);

end Broca.Profiles;
