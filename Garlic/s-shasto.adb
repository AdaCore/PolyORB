------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . S H A R E D _ S T O R A G E                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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

with System.Garlic.Storages; use System.Garlic.Storages;

package body System.Shared_Storage is

   ----------------------
   -- Shared_Var_Close --
   ----------------------

   procedure Shared_Var_Close (Var : in out SIO.Stream_Access)
   is
      VS : Shared_Data_Access;

   begin
      VS := Shared_Data_Access (Var);
      Complete_Request (VS);
   end Shared_Var_Close;

   ---------------------
   -- Shared_Var_Lock --
   ---------------------

   procedure Shared_Var_Lock (Var : in String)
   is
      VS : Shared_Data_Access;
      Ok : Boolean;

   begin
      VS := Lookup_Variable (Var);
      Initiate_Request (VS, Lock, Ok);
   end Shared_Var_Lock;

   ----------------------
   -- Shared_Var_ROpen --
   ----------------------

   function Shared_Var_ROpen (Var : in String) return SIO.Stream_Access
   is
      VS : Shared_Data_Access;
      Ok : Boolean;

   begin
      VS := Lookup_Variable (Var);
      Initiate_Request (VS, Read, Ok);
      if Ok then
         return SIO.Stream_Access (VS);
      else
         return null;
      end if;
   end Shared_Var_ROpen;

   -----------------------
   -- Shared_Var_Unlock --
   -----------------------

   procedure Shared_Var_Unlock (Var : in String)
   is
      VS : Shared_Data_Access;

   begin
      VS := Lookup_Variable (Var);
      Complete_Request (VS);
   end Shared_Var_Unlock;

   ----------------------
   -- Shared_Var_WOpen --
   ----------------------

   function Shared_Var_WOpen (Var : in String) return SIO.Stream_Access
   is
      VS : Shared_Data_Access;
      Ok : Boolean;

   begin
      VS := Lookup_Variable (Var);
      Initiate_Request (VS, Write, Ok);
      return SIO.Stream_Access (VS);
   end Shared_Var_WOpen;

end System.Shared_Storage;
