------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--      S Y S T E M . G A R L I C . S O C K E T S . S E L E C T O R S       --
--                                                                          --
--                                 S p e c                                  --
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

package System.Garlic.Sockets.Selectors is

   type Socket_Set_Type is private;

   procedure Clear (Set : in out Socket_Set_Type; Socket : in Socket_Type);
   procedure Set   (Set : in out Socket_Set_Type; Socket : in Socket_Type);
   procedure Zero  (Set : in out Socket_Set_Type);

   function Empty
     (Set : Socket_Set_Type) return Boolean;

   function Is_Set
     (Set    : Socket_Set_Type;
      Socket : Socket_Type) return Boolean;

   type Microseconds is new Natural;
   Forever : constant Microseconds := Microseconds'Last;


   type Selector_Type is limited private;
   type Selector_Access is access all Selector_Type;

   function Create_Selector return Selector_Access;

   type Selector_Status is (Completed, Expired, Aborted);

   procedure Select_Socket
     (Selector     : access Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : in Microseconds := Forever);

   procedure Abort_Select
     (Selector : access Selector_Type);

private

   --  The two signalling sockets are used to abort a select
   --  operation.

   type Selector_Type is limited record
      R_Sig_Socket : Socket_Type;
      W_Sig_Socket : Socket_Type;
      In_Progress  : Boolean := False;
   end record;

   type Socket_Set_Record;
   type Socket_Set_Type is access all Socket_Set_Record;

end System.Garlic.Sockets.Selectors;
