------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N F I G U R A T I O N . S E R V E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with MOMA.Provider.Message_Pool;
with MOMA.Types;
with PolyORB.Configuration;
with PolyORB.Log;
with PolyORB.MOMA_P.Tools;

package body MOMA.Configuration.Server is

   use MOMA.Types;
   use PolyORB.Configuration;
   use PolyORB.MOMA_P.Tools;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("moma.configuration.server");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   -------------------------
   -- Create_Message_Pool --
   -------------------------

   procedure Create_Message_Pool (Name : in String;
                                  Ref  : out PolyORB.References.Ref)
   is
      MOMA_Obj : constant MOMA.Provider.Message_Pool.Object_Acc
       := new MOMA.Provider.Message_Pool.Object;

   begin
      pragma Debug (O ("Creating Message Pool " & Name));
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Pool.If_Desc,
                        Ref);
   end Create_Message_Pool;

   ---------------------------
   -- Get_Message_Pool_Info --
   ---------------------------

   function Get_Message_Pool_Info (Number : Natural)
                                   return Message_Pool_Info
   is
      Section : constant String
        := "destination" & Natural'Image (Number);

      Pool_Type : constant String := Get_Conf (Section, "type");

      Result : Message_Pool_Info;
   begin
      Result.Name := To_MOMA_String (Get_Conf (Section, "name"));

      pragma Debug (O ("Pool #" & Natural'Image (Number) & " : "
                       & "Name : " & To_Standard_String (Result.Name)
                       & ", Type : " & Pool_Type));

      if Pool_Type = "queue" then
         Result.Pool_Type := Queue;
      elsif Pool_Type = "topic" then
         Result.Pool_Type := Topic;
      else
         raise Program_Error;
         --  XXX should raise something else ...
      end if;

      return Result;
   end Get_Message_Pool_Info;

end MOMA.Configuration.Server;
