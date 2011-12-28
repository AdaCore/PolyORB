------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      POLYORB.GIOP_P.TAGGED_COMPONENTS.ALTERNATE_IIOP_ADDRESS.PRINT       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2011, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Tagged_Components.Print;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with Common;
with Output;

package body PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address.Print is

   use Common;
   use Output;

   procedure Output (Item : Tagged_Component'Class);

   procedure Output_TC (TC : TC_Alternate_IIOP_Address);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.GIOP_P.Tagged_Components.Print.Register
       (Tag_Alternate_IIOP_Address, Output'Access);
   end Initialize;

   ------------
   -- Output --
   ------------

   procedure Output (Item : Tagged_Component'Class) is
   begin
      Output_TC (TC_Alternate_IIOP_Address (Item));
   end Output;

   ---------------
   -- Output_TC --
   ---------------

   procedure Output_TC (TC : TC_Alternate_IIOP_Address) is
   begin
      Inc_Indent;
      Output_Address_Information (TC.Address.all);
      Dec_Indent;
   end Output_TC;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.alternate_iiop_address.print",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => +"tagged_components.alternate_iiop_address",
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address.Print;
