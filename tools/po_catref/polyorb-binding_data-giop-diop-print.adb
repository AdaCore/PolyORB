------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.BINDING_DATA.GIOP.DIOP.PRINT                    --
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
