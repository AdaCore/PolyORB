------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.BINDING_DATA.GIOP.DIOP.PRINT                    --
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
with PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
with PolyORB.Initialization;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Utils.Strings;

with PolyORB.GIOP_P.Tagged_Components.Print;

package body PolyORB.Binding_Data.GIOP.DIOP.Print is

   ------------------------
   -- Print_DIOP_Profile --
   ------------------------

   procedure Print_DIOP_Profile (Prof : Profile_Access) is
      use Common;
      use Output;

      use PolyORB.GIOP_P.Tagged_Components.Print;
      use PolyORB.GIOP_P.Transport_Mechanisms;
      use PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
      use PolyORB.Utils;

      DIOP_Prof : DIOP_Profile_Type renames DIOP_Profile_Type (Prof.all);

   begin
      Inc_Indent;

      Put_Line ("DIOP Version",
                Trimmed_Image (Unsigned_Long_Long (DIOP_Prof.Version_Major))
                & "." &
                Trimmed_Image (Unsigned_Long_Long (DIOP_Prof.Version_Minor)));

      Output_Address_Information
        (Address_Of
         (DIOP_Transport_Mechanism
          (Element (DIOP_Prof.Mechanisms, 0).all.all)));

      Output_Tagged_Components (DIOP_Prof.Components);

      Dec_Indent;
   end Print_DIOP_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Binding_Data.Print.Register
        (Tag_DIOP, Print_DIOP_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.binding_data.diop.print",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.GIOP.DIOP.Print;
