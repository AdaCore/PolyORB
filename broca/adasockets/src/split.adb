-----------------------------------------------------------------------------
--                                                                         --
--                         ADASOCKETS COMPONENTS                           --
--                                                                         --
--                               S P L I T                                 --
--                                                                         --
--                                B o d y                                  --
--                                                                         --
--                        $ReleaseVersion: 0.1.6 $                         --
--                                                                         --
--                        Copyright (C) 1998-2000                          --
--             École Nationale Supérieure des Télécommunications           --
--                                                                         --
--   AdaSockets is free software; you can  redistribute it and/or modify   --
--   it  under terms of the GNU  General  Public License as published by   --
--   the Free Software Foundation; either version 2, or (at your option)   --
--   any later version.   AdaSockets is distributed  in the hope that it   --
--   will be useful, but WITHOUT ANY  WARRANTY; without even the implied   --
--   warranty of MERCHANTABILITY   or FITNESS FOR  A PARTICULAR PURPOSE.   --
--   See the GNU General Public  License  for more details.  You  should   --
--   have received a copy of the  GNU General Public License distributed   --
--   with AdaSockets; see   file COPYING.  If  not,  write  to  the Free   --
--   Software  Foundation, 59   Temple Place -   Suite  330,  Boston, MA   --
--   02111-1307, USA.                                                      --
--                                                                         --
--   As a special exception, if  other  files instantiate generics  from   --
--   this unit, or  you link this  unit with other  files to produce  an   --
--   executable,  this  unit does  not  by  itself cause  the  resulting   --
--   executable to be  covered by the  GNU General Public License.  This   --
--   exception does  not  however invalidate any  other reasons  why the   --
--   executable file might be covered by the GNU Public License.           --
--                                                                         --
--   The main repository for this software is located at:                  --
--       http://www.infres.enst.fr/ANC/                                    --
--                                                                         --
--   If you have any question, please send a mail to                       --
--       Samuel Tardieu <sam@inf.enst.fr>                                  --
--                                                                         --
-----------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with System.IO;        use System.IO;

procedure Split is

   Column     : Natural  := 0;
   Max_Column : constant := 50;

begin
   if Argument_Count /= 1 then
      Put_Line ("Error, usage: split ""text""");
      Set_Exit_Status (1);
   else
      for I in 1 .. Argument (1) 'Length loop
         if Argument (1) (I) = ' ' and then Column >= Max_Column then
            New_Line;
            Column := 0;
         else
            if Column = 0 then
               Put ("--  ");
            end if;
            Put (Argument (1) (I));
            Column := Column + 1;
         end if;
      end loop;
      if Column > 0 then
         New_Line;
      end if;
   end if;
end Split;
