------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                            E V E N T S _ S H                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;    use Ada.Exceptions;
with Menu;              use Menu;

with GLADE_Naming_Implementation;
use GLADE_Naming_Implementation;

with GLADE.Event_Communication;
use GLADE.Event_Communication;

with GLADE.Event_Communication.Implementation;
use GLADE.Event_Communication.Implementation;

with GLADE.Event_Communication.Interface;
use GLADE.Event_Communication.Interface;

with GLADE.Event_Channel_Admin;
use GLADE.Event_Channel_Admin;

with GLADE.Event_Channel_Admin.Implementation;
use GLADE.Event_Channel_Admin.Implementation;

with GLADE.Event_Channel_Admin.Interface;
use GLADE.Event_Channel_Admin.Interface;

procedure Events_Sh is
   Channel          : Event_Channel_Ref;
   Supp_Admin       : Supplier_Admin_Ref;
   Cons_Admin       : Consumer_Admin_Ref;
   Push_Supp        : Push_Supplier_Ref;
   Push_Cons        : Push_Consumer_Ref;
   Pull_Supp        : Pull_Supplier_Ref;
   Pull_Cons        : Pull_Consumer_Ref;
   Proxy_Push_Supp  : Proxy_Push_Supplier_Ref;
   Proxy_Push_Cons  : Proxy_Push_Consumer_Ref;
   Proxy_Pull_Supp  : Proxy_Pull_Supplier_Ref;
   Proxy_Pull_Cons  : Proxy_Pull_Consumer_Ref;

   Word : String_Access;
   Argc : Natural;
begin
   loop
      Argc := Count;
      if Argc > 0 then
         begin
            Word := Argument (1);
            To_Lower (Word);
            if Argc = 1
              and then Word.all = "exit"
            then
               exit;

            elsif Argc = 6
              and then Word.all = "connect"
              and then Argument (5).all = "to"
            then
               Word := Argument (6);
               begin
                  Channel := Resolve (Word.all).Channel;
               exception when others =>
                  Channel := Create;
                  Bind (Word.all, (Channel_K, Channel));
               end;

               Word := Argument (2);

               if Word.all = "push" then

                  Word := Argument (3);
                  if Word.all = "supplier" then

                     Word := Argument (4);
                     begin
                        Push_Supp := Resolve (Word.all).Push_Supp;
                     exception when others =>
                        Push_Supp := Create;
                        Bind (Word.all, (Push_Supp_K, Push_Supp));
                     end;
                     Supp_Admin := For_Suppliers (Channel);
                     Proxy_Push_Cons := Obtain (Supp_Admin);
                     Connect (Proxy_Push_Cons, Push_Supp);

                  elsif Word.all = "consumer" then

                     Word := Argument (4);
                     begin
                        Push_Cons := Resolve (Word.all).Push_Cons;
                     exception when others =>
                        Push_Cons := Create;
                        Bind (Word.all, (Push_Cons_K, Push_Cons));
                     end;
                     Cons_Admin := For_Consumers (Channel);
                     Proxy_Push_Supp := Obtain (Cons_Admin);
                     Connect (Proxy_Push_Supp, Push_Cons);

                  else
                     Ada.Text_IO.Put_Line ("syntax error");
                  end if;

               elsif Word.all = "pull" then

                  Word := Argument (3);
                  if Word.all = "supplier" then

                     Word := Argument (4);
                     begin
                        Pull_Supp := Resolve (Word.all).Pull_Supp;
                     exception when others =>
                        Pull_Supp := Create;
                        Bind (Word.all, (Pull_Supp_K, Pull_Supp));
                     end;
                     Supp_Admin := For_Suppliers (Channel);
                     Proxy_Pull_Cons := Obtain (Supp_Admin);
                     Connect (Proxy_Pull_Cons, Pull_Supp);

                  elsif Word.all = "consumer" then

                     Word := Argument (4);
                     begin
                        Pull_Cons := Resolve (Word.all).Pull_Cons;
                     exception when others =>
                        Pull_Cons := Create;
                        Bind (Word.all, (Pull_Cons_K, Pull_Cons));
                     end;
                     Cons_Admin := For_Consumers (Channel);
                     Proxy_Pull_Supp := Obtain (Cons_Admin);
                     Connect (Proxy_Pull_Supp, Pull_Cons);

                  else
                     Ada.Text_IO.Put_Line ("syntax error");
                  end if;

               else
                  Ada.Text_IO.Put_Line ("syntax error");
               end if;

            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line ("exception: "& Exception_Name (E));
         end;
      end if;
   end loop;
end Events_Sh;
