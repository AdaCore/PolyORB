------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                         T R A C I N G _ T E S T                          --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

package body Tracing_Test is

   protected Lock is
      entry Enter;
      procedure Leave;
   private
      Busy : Boolean := False;
   end Lock;

   protected body Lock is
      entry Enter when not Busy is
      begin
         Busy := True;
      end;
      procedure Leave is
      begin
         Busy := False;
      end;
   end Lock;

   procedure Tracing_Report (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Tracing_Report;

   function Semaphore_String (Semaphore : Semaphore_Access) return String is
   begin
      if Semaphore = null then
         return "<null semaphore>";
      else
         return Semaphore_Name (Semaphore) &
           Positive'Image (Semaphore_Index (Semaphore));
      end if;
   exception
      when E : others =>
         return "(Exception " & Exception_Information (E) & ")";
   end Semaphore_String;

   procedure Tracing_Callback (Actor     : in Semaphore_Access;
                               Operation : in Operation_Type;
                               Data      : in Semaphore_Access) is
   begin
      Lock.Enter;
      Put_Line ("Received operation " & Operation_Type'Image (Operation));
      Put_Line ("From " & Semaphore_String (Actor));
      if Data /= null then
         Put_Line ("To " & Semaphore_String (Data));
      end if;
      New_Line;
      Lock.Leave;
   exception
      when E : others =>
         Put_Line ("Exception " & Exception_Information (E));
   end Tracing_Callback;

end Tracing_Test;
