------------------------------------------------------------------------------
--                                                                          --
--                            GLADE EXAMPLES                                --
--                                                                          --
--                             P R I M E _ 3                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GLADE  is  free software;  you  can redistribute  it  and/or  modify  it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version.  GLADE  is  distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General  Public  License  distributed with GLADE;  see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                GLADE is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Prime_2;
with Prime_4;
with Text_IO;
with Types;
use Types;
package body Prime_3 is

   task Keeper is
      pragma Storage_Size (130000);
      entry Begin_Session
        (Number  : Std_Number;
         Session : Std_Session);
      entry End_Session
        (Session : Std_Session;
         Node    : Std_Node;
         Result  : Boolean);
      entry Initiate
        (Number : Std_Number);
      entry Complete
        (Node   : out Std_Node;
         Result : out Boolean);
   end Keeper;

   procedure Begin_Session
     (Number  : Std_Number;
      Session : Std_Session) is
   begin
      Keeper.Begin_Session (Number, Session);
   end Begin_Session;

   procedure Initiate
     (Number : Std_Number;
      Node   : out Std_Node;
      Result : out Boolean) is
   begin
      Keeper.Initiate (Number);
      Keeper.Complete (Node, Result);
   end Initiate;

   procedure End_Session
     (Session : Std_Session;
      Node    : Std_Node;
      Result  : Boolean) is
   begin
      Keeper.End_Session (Session, Node, Result);
   end End_Session;

   task body Keeper is
      subtype Std_Table_Pos is Std_Session range 1 .. 256;
      Primes  : array (Std_Table_Pos) of Std_Number := (others => 0);
      Last    : Std_Session  := 0;
      First   : Std_Session  := 0;
      Round   : Std_Session  := 1;
      Current : Std_Number;
      Success : Boolean;
      Prev    : Std_Session  := 0;
      Next    : Std_Session  := 0;
      Free    : Boolean      := True;
      Where   : Std_Node;
   begin
      loop
         select
            accept End_Session
               (Session : Std_Session;
        	Node    : Std_Node;
                Result  : Boolean) do
               Prev    := Session;
               Where   := Node;
               Success := Result;
            end End_Session;
            if Prev = 1 then
               Prev := 0;
               Free := True;
            end if;
         or
            when Free =>
               accept Initiate
                 (Number : Std_Number) do
                  Current := Number;
                  Next    := 1;
                  Free    := False;
                  Success := False;
               end Initiate;
         or
            when Free =>
               accept Complete
                 (Node   : out Std_Node;
                  Result : out Boolean) do
                  Result := Success;
                  Node   := Where;
               end Complete;
         or
            accept Begin_Session
              (Number  : Std_Number;
               Session : Std_Session) do
               Current := Number;
               Next    := Session;
            end Begin_Session;
         or terminate;
         end select;
         If Next /= 0 then
            if Last /= 0 then
               if Next > Last then
                  Last := Next;
                  if Round = 1 then
                     Round := Last - First;
                  end if;
                  Primes ((Last-First) / Round+1) := Current;
		  Success := True;
		  Where   := 3;
                  Prev := Next;
               elsif Current rem Primes ((Next-First) / Round+1) /= 0 then
                  Prime_4.Begin_Session (Current, Next + 1);
               else
                  Prev := Next;
               end if;
            else
               First := Next;
               Last  := Next;
               Primes (1) := Current;
	       Success := True;
	       Where   := 3;
               Prev := Next;
            end if;
            Next := 0;
         end if;
         if Prev /= 0 then
            if Prev /= 1 or else Free then
               Prime_2.End_Session (Prev - 1, Where, Success);
               Success := False;
            else
               Free := True;
            end if;
            Prev := 0;
         end if;
      end loop;
   exception when others =>
      Text_Io.Put_Line ("Keeper is dead");
   end;
end Prime_3;
