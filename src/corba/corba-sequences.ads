------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C O R B A . S E Q U E N C E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with PolyORB.Sequences;

package CORBA.Sequences is

   Length_Error  : exception renames PolyORB.Sequences.Length_Error;
   Pattern_Error : exception renames PolyORB.Sequences.Pattern_Error;
   Index_Error   : exception renames PolyORB.Sequences.Index_Error;

   subtype Alignment  is PolyORB.Sequences.Alignment;
   subtype Truncation is PolyORB.Sequences.Truncation;
   subtype Membership is PolyORB.Sequences.Membership;
   subtype Direction  is PolyORB.Sequences.Direction;
   subtype Trim_End   is PolyORB.Sequences.Trim_End;

end CORBA.Sequences;
