------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O P                             --
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

with CORBA;             use CORBA;
with Broca.Buffers;     use Broca.Buffers;

with Broca.Debug;
pragma Elaborate (Broca.Debug);

package body Broca.IOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iop");
   procedure O is new Broca.Debug.Output (Flag);

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

   procedure Find_Best_Profile
     (Profiles : in Profile_Ptr_Array_Ptr;
      Used_Profile_Index : out CORBA.Unsigned_Long;
      Is_Supported_Profile : out Boolean)
   is
      Max_Priority : Profile_Priority := Profile_Priority'First;
      Priority : Profile_Priority;
   begin
      Used_Profile_Index := 0;
      Is_Supported_Profile := False;
      for I in Profiles'Range loop
         Priority := Get_Profile_Priority (Profiles (I).all);
         if Priority > Max_Priority then
            Used_Profile_Index := I;
            Max_Priority := Priority;
            Is_Supported_Profile := True;
         end if;
      end loop;
   end Find_Best_Profile;

end Broca.IOP;



