------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . P R O F I L E S . U N K N O W N                --
--                                                                          --
--                                 S p e c                                  --
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

with Broca.Sequences;

package Broca.Profiles.Unknown is

   pragma Elaborate_Body;

   type Encapsulation_Ptr is access Broca.Buffers.Encapsulation;

   type Unknown_Profile_Type is new Profile_Type with record
      Tag  : Profile_Tag;
      Data : Encapsulation_Ptr := null;
   end record;

   type Unknown_Profile_Access is access Unknown_Profile_Type;

   function Get_Object_Key
     (Profile : Unknown_Profile_Type)
     return Broca.Sequences.Octet_Sequence;

   function Find_Connection
     (Profile : access Unknown_Profile_Type)
     return Connection_Ptr;

   function Get_Profile_Tag
     (Profile : Unknown_Profile_Type)
     return Profile_Tag;

   function Get_Profile_Priority
     (Profile : in Unknown_Profile_Type)
     return Profile_Priority;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type);

   procedure Finalize
     (X : in out Unknown_Profile_Type);

end Broca.Profiles.Unknown;
