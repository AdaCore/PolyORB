------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      C O R B A . S E Q U E N C E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Sequences;

package CORBA.Sequences is

   Length_Error : exception renames Standard.Sequences.Length_Error;
   Pattern_Error : exception renames Standard.Sequences.Pattern_Error;
   Index_Error : exception renames Standard.Sequences.Index_Error;

   subtype Alignment is Standard.Sequences.Alignment;
   function Left return Alignment renames Standard.Sequences.Left;
   function Right return Alignment renames Standard.Sequences.Right;
   function Center return Alignment renames Standard.Sequences.Center;

   subtype Truncation is Standard.Sequences.Truncation;
   function Left return Truncation renames Standard.Sequences.Left;
   function Right return Truncation renames Standard.Sequences.Right;
   function Error return Truncation renames Standard.Sequences.Error;

   subtype Membership is Standard.Sequences.Membership;
   function Inside return Membership renames Standard.Sequences.Inside;
   function Outside return Membership renames Standard.Sequences.Outside;

   subtype Direction is Standard.Sequences.Direction;
   function Forward return Direction renames Standard.Sequences.Forward;
   function Backward return Direction renames Standard.Sequences.Backward;

   subtype Trim_End is Standard.Sequences.Trim_End;
   function Left return Trim_End renames Standard.Sequences.Left;
   function Right return Trim_End renames Standard.Sequences.Right;

end CORBA.Sequences;
