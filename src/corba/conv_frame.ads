------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O N V _ F R A M E                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006 Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Sequences.Unbounded;

package CONV_FRAME is

   --  CodeSetId type

   type CodeSetId is new CORBA.Unsigned_Long;

   --  CodeSetComponent structure

   package IDL_SEQUENCE_CONV_FRAME_CodeSetId is
     new CORBA.Sequences.Unbounded (CodeSetId);

   type CodeSetComponent is record
      Native_Code_Set      : CodeSetId;
      Conversion_Code_Sets : IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence;
   end record;

   --  CodeSetComponentInfo structure

   type CodeSetComponentInfo is record
      ForCharData  : CodeSetComponent;
      ForWcharData : CodeSetComponent;
   end record;

   --  CodeSetContext structure

   type CodeSetContext is record
      Char_Data  : CodeSetId;
      Wchar_Data : CodeSetId;
   end record;

   --  Repository Ids

   Repository_Id                      : constant Standard.String
     := "IDL:omg.org/CONV_FRAME:1.0";

   CodeSetComponent_Repository_Id     : constant Standard.String
     := "IDL:omg.org/CONV_FRAME/CodeSetComponent:1.0";

   CodeSetComponentInfo_Repository_Id : constant Standard.String
     := "IDL:omg.org/CONV_FRAME/CodeSetComponentInfo:1.0";

   CodeSetContext_Repository_Id       : constant Standard.String
     := "IDL:omg.org/CONV_FRAME/CodeSetContext:1.0";

   CodeSetId_Repository_Id            : constant Standard.String
     := "IDL:omg.org/CONV_FRAME/CodeSetId:1.0";

end CONV_FRAME;
