------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M O M A . C O N N E C T I O N S                      --
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

package body MOMA.Connections is

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      null;
   end Close;

   -----------------------
   -- Create_Connection --
   -----------------------

   function Create_Connection
      (Factory : MOMA.Connection_Factories.Connection_Factory)
      return Connection
   is
      New_Connection : Connection;

   begin
      Set_Ref (New_Connection, MOMA.Connection_Factories.Get_Ref (Factory));

      return New_Connection;
   end Create_Connection;

   function Create_Connection
      (Factory   : MOMA.Connection_Factories.Connection_Factory;
       Username  : String;
       Password  : String)
      return Connection
   is
   begin
      raise Program_Error;
      pragma Warnings (Off);
      return Create_Connection (Factory, Username, Password);
      pragma Warnings (On);
   end Create_Connection;

   -------------------
   -- Get_Client_Id --
   -------------------

   function Get_Client_Id
     (Self : Connection)
     return MOMA.Types.String is
   begin
      return Self.Client_Id;
   end Get_Client_Id;

   -------------------
   -- Set_Client_Id --
   -------------------

   procedure Set_Client_Id
     (Self      : in out Connection;
      Client_Id :        MOMA.Types.String) is
   begin
      Self.Client_Id := Client_Id;
   end Set_Client_Id;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref
     (Self : Connection)
     return MOMA.Types.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref
     (Self : in out Connection;
      Ref  :        MOMA.Types.Ref) is
   begin
      Self.Ref := Ref;
   end Set_Ref;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      null;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      null;
   end Stop;

   -------------------
   -- Get_Meta_Data --
   -------------------

   function Get_Meta_Data return MOMA.Types.Meta_Data is
   begin
      return 0;
   end Get_Meta_Data;

end MOMA.Connections;
