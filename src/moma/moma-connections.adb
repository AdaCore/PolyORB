------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M O M A . C O N N E C T I O N S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with MOMA.Types;

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
      raise PolyORB.Not_Implemented;
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
     return PolyORB.References.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref
     (Self : in out Connection;
      Ref  :        PolyORB.References.Ref) is
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
