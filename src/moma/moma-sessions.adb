------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . S E S S I O N S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body MOMA.Sessions is

   ------------
   --  Close --
   ------------

   procedure Close is
   begin
      null;
   end Close;

   -------------
   --  Commit --
   -------------

   procedure Commit is
   begin
      null;
   end Commit;

   ---------------------------
   --  Create_Array_Message --
   ---------------------------

   function Create_Array_Message return Messages.MArrays.MArray is
   begin
      pragma Warnings (Off);
      return Create_Array_Message;
      pragma Warnings (On);
   end Create_Array_Message;

   --------------------------
   --  Create_Byte_Message --
   --------------------------

   function Create_Byte_Message return Messages.MBytes.MByte is
   begin
      pragma Warnings (Off);
      return Create_Byte_Message;
      pragma Warnings (On);
   end Create_Byte_Message;

   ----------------------------
   --  Create_Record_Message --
   ----------------------------

   function Create_Record_Message return Messages.MRecords.MRecord is
   begin
      pragma Warnings (Off);
      return Create_Record_Message;
      pragma Warnings (On);
   end Create_Record_Message;

   ----------------------------
   --  Create_Stream_Message --
   ----------------------------

   function Create_Stream_Message return Messages.MStreams.MStream is
   begin
      pragma Warnings (Off);
      return Create_Stream_Message;
      pragma Warnings (On);
   end Create_Stream_Message;

   ---------------------
   --  Get_Transacted --
   ---------------------

   function Get_Transacted return Boolean is
   begin
      pragma Warnings (Off);
      return Get_Transacted;
      pragma Warnings (On);
   end Get_Transacted;

   --------------
   --  Recover --
   --------------

   procedure Recover is
   begin
      null;
   end Recover;

   ---------------
   --  Rollback --
   ---------------

   procedure Rollback is
   begin
      null;
   end Rollback;

end MOMA.Sessions;

