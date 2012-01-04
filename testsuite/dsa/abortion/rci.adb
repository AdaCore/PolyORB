------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  R C I                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

with Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;

package body RCI is

   task Keep_Alive is
      entry Allow_Terminate;
   end Keep_Alive;

   task body Keep_Alive is
   begin
      accept Allow_Terminate;
   end Keep_Alive;

   protected Barrier is
      entry Wait;
      --  First call is passing, subsequent block for ever

      function Blocked_Calls return Natural;
   private
      Passing : Boolean := True;
   end Barrier;

   protected body Barrier is
      entry Wait when Passing is
      begin
         Passing := False;
      end Wait;

      function Blocked_Calls return Natural is
      begin
         return Wait'Count;
      end Blocked_Calls;
   end Barrier;

   type Witness (Call_Id : Integer) is
     new Ada.Finalization.Limited_Controlled with
   record
      Completed : Boolean := False;
   end record;

   procedure Initialize (X : in out Witness);
   procedure Finalize (X : in out Witness);

   procedure Initialize (X : in out Witness) is
   begin
      Put_Line ("call" & X.Call_Id'Img & " started");
   end Initialize;

   procedure Finalize (X : in out Witness) is
   begin
      Put_Line ("call" & X.Call_Id'Img
                & " terminated, completed = " & X.Completed'Img);
   end Finalize;

   procedure Block_On_Entry (Call_Id : Integer) is
      W : Witness (Call_Id);
   begin
      Barrier.Wait;
      W.Completed := True;
   end Block_On_Entry;

   function Blocked_Calls return Natural is
   begin
      Put_Line ("Blocked_Calls: enter");
      return Barrier.Blocked_Calls;
   end Blocked_Calls;

   procedure Allow_Terminate is
   begin
      Keep_Alive.Allow_Terminate;
   end Allow_Terminate;

   type Pkg_Witness is
      new Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (X : in out Pkg_Witness);
   procedure Finalize (X : in out Pkg_Witness);

   procedure Initialize (X : in out Pkg_Witness) is
   begin
      Put_Line ("Pkg_Witness: initialize");
   end Initialize;

   procedure Finalize (X : in out Pkg_Witness) is
   begin
      Put_Line ("Pkg_Witness: finalize");
   end Finalize;

   PW : Pkg_Witness;

end RCI;
