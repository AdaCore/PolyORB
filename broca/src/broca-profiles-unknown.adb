------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . P R O F I L E S . U N K N O W N                --
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

with Broca.CDR;        use Broca.CDR;
with Broca.Exceptions;

package body Broca.Profiles.Unknown is

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Unknown_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Broca.Buffers.Encapsulation, Encapsulation_Ptr);
   begin
      Free (X.Data);
   end Finalize;

   ---------------------
   -- Find_Connection --
   ---------------------

   function Find_Connection
     (Profile : access Unknown_Profile_Type)
     return Connection_Ptr is
   begin
      Broca.Exceptions.Raise_Bad_Param;
      return null;
   end Find_Connection;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Unknown_Profile_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      Broca.Exceptions.Raise_Bad_Param;
      return Broca.Sequences.Null_Sequence;
   end Get_Object_Key;

   --------------------------
   -- Get_Profile_Priority --
   --------------------------

   function Get_Profile_Priority
     (Profile : in Unknown_Profile_Type)
     return Profile_Priority is
   begin
      return Profile_Priority'First;
   end Get_Profile_Priority;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : Unknown_Profile_Type)
     return Profile_Tag is
   begin
      return Profile.Tag;
   end Get_Profile_Tag;

   ---------------------------
   -- Marshall_Profile_Body --
   ---------------------------

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type) is
   begin
      Marshall (Buffer, Profile.Data.all);
   end Marshall_Profile_Body;

end Broca.Profiles.Unknown;
