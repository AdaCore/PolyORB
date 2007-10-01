------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Security.Credentials is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.security.credentials");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Credentials_Type_Info is record
      Credentials_Type : PolyORB.Types.String;
      Constructor      : Credentials_Constructor;
   end record;

   package Credentials_Type_Info_Lists is
     new PolyORB.Utils.Chained_Lists (Credentials_Type_Info);

   Registry : Credentials_Type_Info_Lists.List;

   ------------------------
   -- Create_Credentials --
   ------------------------

   function Create_Credentials
     (Credentials_Type : String;
      Section_Name     : String)
      return Credentials_Ref'Class
   is
      use Credentials_Type_Info_Lists;
      use type PolyORB.Types.String;

      Result : Credentials_Ref;
      Aux    : Credentials_Access := null;
      Iter   : Iterator           := First (Registry);

   begin
      while not Last (Iter) loop
         if Value (Iter).Credentials_Type = Credentials_Type then
            Aux := Value (Iter).Constructor (Section_Name);
            exit;
         end if;

         Next (Iter);
      end loop;

      Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Aux));

      return Result;
   end Create_Credentials;

   --------------
   -- Register --
   --------------

   procedure Register
     (Credentials_Type : String;
      Constructor      : Credentials_Constructor)
   is
      use Credentials_Type_Info_Lists;

   begin
      pragma Debug
        (O ("Register credentials type: '" & Credentials_Type & '''));

      Append
        (Registry,
         (PolyORB.Types.To_PolyORB_String (Credentials_Type), Constructor));
   end Register;

end PolyORB.Security.Credentials;
