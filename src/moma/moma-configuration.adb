------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   M O M A . C O N F I G U R A T I O N                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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
with PolyORB.Parameters.File;

package body MOMA.Configuration is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("moma.configuration");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------------
   -- Load_Configuration_File --
   -----------------------------

   procedure Load_Configuration_File (Conf_File_Name : String) is
   begin
      PolyORB.Parameters.File.Load_Configuration_File (Conf_File_Name);
   end Load_Configuration_File;

   ----------------------
   -- Get_Message_Pool --
   ----------------------

   function Get_Message_Pool
     (Number : Natural)
     return MOMA.Types.Message_Pool
   is
      use PolyORB.Parameters;
      use MOMA.Types;

      Section : constant String := "destination" & Natural'Image (Number);

      Pool_S : constant String := Get_Conf (Section, "type");
      Persistent_S : constant String := Get_Conf (Section, "persistent");

      Result : Message_Pool;
   begin
      Set_Name (Result, To_MOMA_String (Get_Conf (Section, "name")));

      pragma Debug (C, O ("Pool #" & Natural'Image (Number) & " : "
                       & "Name : " & To_Standard_String (Get_Name (Result))
                       & ", Type : " & Pool_S
                       & ", Persistent : " & Persistent_S));

      if Pool_S = "queue" then
         Set_Type (Result, Queue);

      elsif Pool_S = "topic" then
         Set_Type (Result, Topic);

      else
         raise Program_Error;
         --  XXX should raise something else ...
      end if;

      if Persistent_S = "none" then
         Set_Persistence (Result, None);

      elsif Persistent_S = "file" then
         Set_Persistence (Result, MOMA.Types.File);

      else
         raise Program_Error;
         --  XXX should raise something else ...
      end if;

      return Result;
   end Get_Message_Pool;

end MOMA.Configuration;
