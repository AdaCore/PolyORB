------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A U T O _ P R I N T                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

with Ada.Exceptions;
with Ada.Text_IO;

with CosEventComm.PushConsumer.Impl;

package body Auto_Print is

   use Ada.Text_IO;
   use Ada.Exceptions;

   use CORBA;
   use CosEventComm.PushConsumer.Impl;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   T_Initialized : Boolean := False;

   procedure Ensure_Initialization is
   begin
      if T_Initialized then
         return;
      end if;
      Create (Session_Mutex);
      Create (Session_Taken);
      T_Initialized := True;
   end Ensure_Initialization;

   -------------------------
   -- Thread Auto_Display --
   -------------------------

   procedure  Auto_Display
   is
      Got_Msg : CORBA.Boolean;
      Msg     : CORBA.Any;
      Ptr     : PushConsumer.Impl.Object_Ptr;

   begin
      Ptr := PushConsumer.Impl.Object_Ptr (A_S);
      Enter (Session_Mutex);
      Signal (Session_Taken);
      Leave (Session_Mutex);

      Put_Line ("AutoDisplay setup");
      loop
         exit when EndDisplay;

         delay 0.1;

         Try_Pull (Ptr, Got_Msg, Msg);

         if Got_Msg then
            Ada.Text_IO.Put_Line (To_Standard_String (From_Any (Msg)));
         end if;
      end loop;

      EndDisplay := False;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("raised " & Exception_Name (E));
         Ada.Text_IO.Put_Line (Exception_Message (E));
         Ada.Text_IO.Put_Line (Exception_Information (E));
   end Auto_Display;

end Auto_Print;
