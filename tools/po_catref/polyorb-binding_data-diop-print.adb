------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . B I N D I N G _ D A T A . D I O P . P R I N T       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Common;
with Output;

with PolyORB.Utils;

with PolyORB.GIOP_P.Tagged_Components.Print;

package body PolyORB.Binding_Data.DIOP.Print is

   ------------------------
   -- Print_DIOP_Profile --
   ------------------------

   procedure Print_DIOP_Profile (Prof : Profile_Access) is
      use Common;
      use Output;

      use PolyORB.Utils;

      use PolyORB.GIOP_P.Tagged_Components.Print;

      DIOP_Prof : DIOP_Profile_Type renames DIOP_Profile_Type (Prof.all);

   begin
      Inc_Indent;

      Put_Line ("DIOP Version",
                Trimmed_Image (Integer (DIOP_Prof.Version_Major))
                & "." & Trimmed_Image (Integer (DIOP_Prof.Version_Minor)));

      Output_Address_Information (DIOP_Prof.Address);

      Output_Tagged_Components (DIOP_Prof.Components);

      Dec_Indent;
   end Print_DIOP_Profile;

end PolyORB.Binding_Data.DIOP.Print;
