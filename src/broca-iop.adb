------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O P                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.10 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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
with Broca.CDR; use Broca.CDR;
with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

with CORBA;             use CORBA;

package body Broca.IOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.iop");
   procedure O is new Broca.Debug.Output (Flag);

   function Get_Request_Id (Connection : access Connection_Type)
                            return CORBA.Unsigned_Long
   is
      Res : CORBA.Unsigned_Long;
   begin
      Res := Connection.Request_Id;
      Connection.Request_Id := Connection.Request_Id + 1;
      return Res;
   end Get_Request_Id;

   ---------------------
   -- Decapsulate_IOR --
   ---------------------

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
         declare
            Profile    : constant Profile_Tag
              := Unmarshall (Buffer);
         begin
            Callbacks (Profile).Unmarshall_Profile_Body
              (Buffer, Profiles (N));
         end;
      end loop;
   end Decapsulate_IOR;

   ---------------------
   -- Encapsulate_IOR --
   ---------------------

   procedure Encapsulate_IOR
     (Buffer   : access Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr) is
   begin

      Marshall (Buffer, Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profiles'Length));

      for N in Profiles'Range loop
         Marshall (Buffer, Get_Profile_Tag (Profiles (N).all));
         Callbacks (Get_Profile_Tag (Profiles (N).all)).Marshall_Profile_Body
           (Buffer, Profiles (N));
      end loop;
   end Encapsulate_IOR;

   --------------
   -- Register --
   --------------

   procedure Register
     (Profile     : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type) is
   begin
      Callbacks (Profile).Marshall_Profile_Body := Marshall_Profile_Body;
      Callbacks (Profile).Unmarshall_Profile_Body := Unmarshall_Profile_Body;
   end Register;

end Broca.IOP;
