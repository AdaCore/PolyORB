------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . P R O F I L E S . T C                     --
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

with Broca.Opaque;
with Broca.Sequences;

package Broca.Profiles.TC is

   pragma Elaborate_Body;

   use Broca.Opaque;

   type Component_Id is new CORBA.Unsigned_Long;

   type Tagged_Component_Type (Encapsulation_Size : Positive_Index_Type) is
      record
         Tag : Component_Id;
         Component_Data : Octet_Array (1 .. Encapsulation_Size);
      end record;

   type Tagged_Component_Access is access Tagged_Component_Type;

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Tagged_Component : Tagged_Component_Type);

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
      return Tagged_Component_Access;

   type Tagged_Component_Array is
      array (CORBA.Unsigned_Long range <>) of Tagged_Component_Access;

   type Tagged_Components_Ptr is access Tagged_Component_Array;

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Components : in Tagged_Component_Array);

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Tagged_Components_Ptr;

   type Multiple_Component_Profile_Type is new Profile_Type with
     record
        Components : Tagged_Components_Ptr := null;
     end record;

   procedure Finalization
     (Profile : in out Multiple_Component_Profile_Type);

   function Get_Object_Key
     (Profile : Multiple_Component_Profile_Type)
     return Broca.Sequences.Octet_Sequence;

   function Find_Connection
     (Profile : access Multiple_Component_Profile_Type)
     return Connection_Ptr;

   function Get_Profile_Tag
     (Profile : Multiple_Component_Profile_Type)
      return Profile_Tag;

   function Get_Profile_Priority
     (Profile : in Multiple_Component_Profile_Type)
     return Profile_Priority;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Multiple_Component_Profile_Type);

end Broca.Profiles.TC;
