------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            I R _ S E R V E R                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

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
