------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          I R _ A B _ N A M E S                           --
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

with PO_COS_Naming;
with PolyORB.CORBA_P.Server_Tools;
with PolyORB.If_Descriptors;
with PolyORB.If_Descriptors.CORBA_IR;

procedure IR_AB_Names is
   procedure IR_AB_Names_Setup;
   procedure IR_AB_Names_Setup is
   begin
      PolyORB.If_Descriptors.Default_If_Descriptor
        := new PolyORB.If_Descriptors.CORBA_IR.IR_If_Descriptor;
   end IR_AB_Names_Setup;
begin
   PolyORB.CORBA_P.Server_Tools.Initiate_Server_Hook
     := IR_AB_Names_Setup'Unrestricted_Access;
   PO_COS_Naming;
end IR_AB_Names;
