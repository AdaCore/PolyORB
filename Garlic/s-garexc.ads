------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . E X C E P T I O N S              --
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

with Ada.Exceptions;

with GNAT.OS_Lib;

package System.Garlic.Exceptions is

   pragma Elaborate_Body;

   procedure Raise_With_Errno (Id : in Ada.Exceptions.Exception_Id);
   pragma Inline (Raise_With_Errno);
   --  Raise an exception with a message corresponding to errno

   procedure Raise_Communication_Error (Msg : in String := "");
   pragma No_Return (Raise_Communication_Error);
   pragma Inline (Raise_Communication_Error);
   --  Idem, but with the specific exception System.RPC.Communication_Error.
   --  If an alternate message is given, it will be used instead.

   type Error_Type is limited private;
   function Found (Error : Error_Type) return Boolean;
   procedure Throw (Error : in out Error_Type; Message : in String);
   procedure Catch (Error : in out Error_Type);
   procedure Raise_Communication_Error (Error : in out Error_Type);
   function Content (Error : access Error_Type) return String;
   --  Error type and associated primitives. By default, an Error_Type is
   --  not considered as being an error until Throw has been called.
   --  Catch, Raise_Communication_Error and Content cancel the error.

private

   type Error_Type is new GNAT.OS_Lib.String_Access;

end System.Garlic.Exceptions;
