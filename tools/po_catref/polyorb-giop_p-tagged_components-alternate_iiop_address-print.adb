------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      POLYORB.GIOP_P.TAGGED_COMPONENTS.ALTERNATE_IIOP_ADDRESS.PRINT       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Tagged_Components.Print;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with PO_Catref.Common;
with PO_Catref.Output;

package body PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address.Print is

   use PO_Catref.Common;
   use PO_Catref.Output;

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
