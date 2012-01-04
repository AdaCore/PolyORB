------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.BINDING_DATA.GIOP.UIPMC.PRINT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Common;
with Output;

with PolyORB.Binding_Data.Print;
with PolyORB.GIOP_P.Transport_Mechanisms.UIPMC;
with PolyORB.Initialization;
with PolyORB.MIOP_P.Groups;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Utils.Strings;

with PolyORB.GIOP_P.Tagged_Components.Print;

package body PolyORB.Binding_Data.GIOP.UIPMC.Print is

   -------------------------
   -- Print_UIPMC_Profile --
   -------------------------

   procedure Print_UIPMC_Profile (Prof : Profile_Access) is
      use Common;
      use Output;

      use PolyORB.Utils;

      use PolyORB.GIOP_P.Tagged_Components.Print;
      use PolyORB.GIOP_P.Transport_Mechanisms;
      use PolyORB.GIOP_P.Transport_Mechanisms.UIPMC;
      use PolyORB.MIOP_P.Groups;

      UIPMC_Prof : UIPMC_Profile_Type renames UIPMC_Profile_Type (Prof.all);

   begin
      Inc_Indent;

      Put_Line ("UIPMC Version",
                Trimmed_Image (Unsigned_Long_Long (UIPMC_Prof.Version_Major))
                & "." & Trimmed_Image (Unsigned_Long_Long
                                       (UIPMC_Prof.Version_Minor)));

      Output_Address_Information
        (Address_Of
         (UIPMC_Transport_Mechanism
          (Element (UIPMC_Prof.Mechanisms, 0).all.all)));

      Put_Line ("Group info",
                PolyORB.MIOP_P.Groups.Image (UIPMC_Prof.G_I.all));

      Output_Tagged_Components (UIPMC_Prof.Components);

      Dec_Indent;
   end Print_UIPMC_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Binding_Data.Print.Register
        (Tag_UIPMC, Print_UIPMC_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.binding_data.uipmc.print",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.GIOP.UIPMC.Print;
