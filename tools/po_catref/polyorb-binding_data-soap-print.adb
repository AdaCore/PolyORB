------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . S O A P . P R I N T       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
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

with PO_Catref.Common;
with PO_Catref.Output;

with PolyORB.Binding_Data.Print;
with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.SOAP.Print is

   ------------------------
   -- Print_SOAP_Profile --
   ------------------------

   procedure Print_SOAP_Profile (Prof : Profile_Access) is
      use PO_Catref.Common;
      use PO_Catref.Output;

      SOAP_Prof : SOAP_Profile_Type renames SOAP_Profile_Type (Prof.all);

   begin
      Inc_Indent;

      Put_Line ("SOAP", "(no version information)");

      Output_Address_Information (SOAP_Prof.Address.all);

      Put_Line ("SOAP URI", PolyORB.Types.To_String (SOAP_Prof.URI_Path));

      Output_Object_Information (SOAP_Prof.Object_Id.all);

      Dec_Indent;
   end Print_SOAP_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Binding_Data.Print.Register
        (Tag_SOAP, Print_SOAP_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.binding_data.soap.print",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Binding_Data.SOAP.Print;
