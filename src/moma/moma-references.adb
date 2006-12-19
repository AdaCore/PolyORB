------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      M O M A . R E F E R E N C E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with PolyORB.References.IOR;
with PolyORB.Services.Naming.Tools;

package body MOMA.References is

   -------------------------------
   -- Initialize_Naming_Service --
   -------------------------------

   procedure Initialize_Naming_Service (Naming_Ref : Standard.String) is
      Ref : MOMA.Types.Ref;

   begin
      PolyORB.References.String_To_Object (Naming_Ref, Ref);
      PolyORB.Services.Naming.Tools.Init (Ref);
   end Initialize_Naming_Service;

   -------------------
   -- Register_Name --
   -------------------

   procedure Register_Name
     (Name   : String;
      Ref    : MOMA.Types.Ref;
      Rebind : Boolean := False;
      Sep    : Character := '/')
     renames PolyORB.Services.Naming.Tools.Register;

   ------------
   -- Locate --
   ------------

   function Locate
     (IOR_Or_Name : String;
      Sep         : Character := '/')
     return MOMA.Types.Ref
     renames PolyORB.Services.Naming.Tools.Locate;

   -----------------------------
   -- Reference_To_IOR_String --
   -----------------------------

   function Reference_To_IOR_String
     (Ref : MOMA.Types.Ref)
     return Standard.String
   is
   begin
      return PolyORB.References.IOR.Object_To_String (Ref);
   end Reference_To_IOR_String;

   -------------------------
   -- String_To_Reference --
   -------------------------

   procedure String_To_Reference
     (S   : Standard.String;
      Ref : out MOMA.Types.Ref)
     renames PolyORB.References.String_To_Object;

end MOMA.References;
