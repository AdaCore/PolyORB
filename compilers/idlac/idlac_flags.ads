------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          I D L A C _ F L A G S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

package Idlac_Flags is

   pragma Preelaborate;

   type Encoding is (ISO_Latin_1, UTF_8);

   Generate_Server_Code   : Boolean  := False;
   Generate_Client_Code   : Boolean  := False;
   Generate_Impl_Template : Boolean  := False;
   Keep_Temporary_Files   : Boolean  := False;
   Preprocess_Only        : Boolean  := False;
   To_Stdout              : Boolean  := False;
   Verbose                : Boolean  := False;
   Generate_Delegate      : Boolean  := False;
   Generate_IR            : Boolean  := False;
   Character_Encoding     : Encoding := ISO_Latin_1;

end Idlac_Flags;
