------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . E X C E P T I O N S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  Errors management for the DSA Application Personality of PolyORB.

with PolyORB.Any;
with PolyORB.Errors;

package PolyORB.DSA_P.Exceptions is

   function Exception_Repository_Id (Name, Version : String) return String;
   --  Build a repository ID from an Ada exception name and unit version

   procedure Raise_From_Error
     (Error : in out PolyORB.Errors.Error_Container);
   pragma No_Return (Raise_From_Error);
   --  Raise a DSA specific exception from the data in 'Error'

   procedure Raise_From_Any
     (Occurrence : Any.Any;
      Msg        : String := "<remote exception>");
   pragma No_Return (Raise_From_Any);
   --  Raise a DSA specific exception from the data in Occurrence, with an
   --  optional Exception_Message.

end PolyORB.DSA_P.Exceptions;
