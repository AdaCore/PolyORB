-----------------------------------------------------------------------------
--                                                                         --
--                         ADASOCKETS COMPONENTS                           --
--                                                                         --
--                               M U L T I                                 --
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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with Sockets.Multicast; use Sockets, Sockets.Multicast;

procedure Multi is

   Sock : constant Multicast_Socket_FD :=
     Create_Multicast_Socket ("224.13.194.161", 4161);

   task Send_Packets;

   task body Send_Packets is
      Message : constant String := Argument (1);
   begin
      for I in 1 .. 30 loop
         delay 2.0;
         Put_Line ("Emitting """ & Message & """");
         Put_Line (Sock, Message);
      end loop;
   exception
      when E : others =>
         Put_Line ("Exception raised in Send_Packets task:" &
                   Exception_Information (E));
   end Send_Packets;

begin
   loop
      Put_Line ("Waiting for input");
      Put_Line ("Received """ & Get (Sock) & """");
   end loop;
exception
   when E : others =>
      Put_Line ("Exception raised in main task:" &
                Exception_Information (E));
      if Argument_Count < 1 then
         Put_Line ("Btw, usage is: multi ""string to send""");
      end if;
end Multi;
