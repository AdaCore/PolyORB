------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I R _ E V O L U T E D P                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.Server_Tools;
with EvolutedP_CORBA;
with DSA_Server.IR_Info;
with DSA_Common.Penpal_Type.IR_Info;

procedure IR_Evolutedp is
   procedure Register_IR_Info;
   procedure Register_IR_Info is
   begin
      DSA_Common.Penpal_Type.IR_Info.Register_IR_Info;
      DSA_Server.IR_Info.Register_IR_Info;
   end Register_IR_Info;
begin
   PolyORB.CORBA_P.Server_Tools.Initiate_Server_Hook
     := Register_IR_Info'Unrestricted_Access;
   EvolutedP_CORBA;
end IR_Evolutedp;
