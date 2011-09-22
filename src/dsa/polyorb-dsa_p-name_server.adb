------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . D S A _ P . N A M E _ S E R V E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2009-2011, Free Software Foundation, Inc.          --
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

--  Implementation note: we currently use a hand-coded minimal servant. We
--  should replace it with a RACW at some point.

with Ada.Environment_Variables;

with PolyORB.DSA_P.Exceptions;
with PolyORB.DSA_P.Initialization;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Servants;
with PolyORB.Services.Naming.NamingContext.Servant;
with PolyORB.Setup;
with PolyORB.Utils.Strings.Lists;

package body PolyORB.DSA_P.Name_Server is

   use PolyORB.DSA_P.Exceptions;
   use PolyORB.DSA_P.Initialization;
   use PolyORB.Errors;
   use PolyORB.Initialization;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.References.IOR;
   use PolyORB.Services.Naming.NamingContext.Servant;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.name_server");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package NC renames PolyORB.Services.Naming.NamingContext.Servant;

   Root_NC     : NC.Object_Ptr;
   Root_NC_Ref : PolyORB.References.Ref;

   procedure Initialize_Naming_Context;
   --  Set up the name server and set Root_NC_Ref

   -------------------------------
   -- Initialize_Naming_Context --
   -------------------------------

   procedure Initialize_Naming_Context is
      use PolyORB.POA;

      Root_POA        : constant Obj_Adapter_Access :=
                           Obj_Adapter_Access
                             (Object_Adapter (PolyORB.Setup.The_ORB));
      Oid             : Object_Id_Access;
      Error           : Errors.Error_Container;
      Type_Id         : constant Standard.String := "dsa:NAMING";
      Root_NC_Servant : PolyORB.Servants.Servant_Access;
   begin
      Root_NC := NC.Create;
      Root_NC_Servant := To_PolyORB_Servant (Root_NC);

      Export (Root_POA, Root_NC_Servant, null, Oid, Error);
      if Found (Error) then
         Raise_From_Error (Error);
      end if;

      PolyORB.ORB.Create_Reference
        (PolyORB.Setup.The_ORB, Oid, Type_Id, Root_NC_Ref);

      PolyORB.POA_Types.Free (Oid);

      declare
         Nameserver_Str : constant String := Object_To_String (Root_NC_Ref);
      begin
         Ada.Environment_Variables.Set
           ("POLYORB_DSA_NAME_SERVICE", Nameserver_Str);
         pragma Debug (C, O ("POLYORB_DSA_NAME_SERVICE=" & Nameserver_Str));
      end;

      Initiate_Well_Known_Service (Root_NC_Servant, "_NameService");
   end Initialize_Naming_Context;

   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"dsa.name_server",
       Conflicts => Empty,
       Depends   => +"orb" & "poa" & "access_points",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize_Naming_Context'Access,
       Shutdown  => null));
end PolyORB.DSA_P.Name_Server;
