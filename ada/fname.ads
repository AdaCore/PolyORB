------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the association between source file names and
--  unit names as defined in package Uname.

with Types; use Types;

package Fname is

   --  Note: this package spec does not depend on the Uname spec in the Ada
   --  sense, but the comments and description of the semantics do depend on
   --  the conventions established by Uname.

   ---------------------------
   -- File Name Conventions --
   ---------------------------

   --  GNAT requires that there be a one to one correspondence between source
   --  file names (as used in the Osint package interface) and unit names as
   --  defined by the Uname package. This correspondence is defined by the
   --  two subprograms defined here in the Fname package.

   --  The body of this package is potentially system dependent, since file
   --  naming conventions do differ from operating system to operating system.
   --  However, the code in the body of Fname does not typically require any
   --  operating system interface, and furthermore, we choose a convention
   --  that is likely to be widely implementable, and certainly is one that
   --  can be shared between Unix, DOS, NT, Mac OS and OS/2.

   --  Since we do expect this convention to be followed widely, and since
   --  Osint depends on the convention, it is described here in the Spec.
   --  However, no unit (other than Osint) in any way depends on the choices
   --  described here.

   --  Unit names are the Ada names, with all lower case letters (except for
   --  the use of upper case letters for encoding and for internal names,
   --  see package Namet for further details), and a suffix that is either
   --  %b or %s for bodies and specs respectively. This is the convention
   --  described and implemented in package Uname.

   --  Source file names are obtained by taking the decoded unit name (i.e.
   --  with Uhh and Whhhh sequences decoded to the ESC sequence or literal
   --  upper half character), excluding the %b or %s, and replacing the
   --  periods with minus signs. The extension is either .ads for a spec,
   --  or .adb for a body (or subunit).

   --  Examples of these rules are:

   --    Unit                           Unit name           File name

   --    Packge_Scan (spec)             packge_scan%s      packge_scan.ads
   --    Packge_Scan (body)             packge_scan%b      packge_scan.adb
   --    Scn.Nlit (subunit)             scn.nlit%b         scn-nlit.adb
   --    Child.Pkg (child unit spec)    child.pkg%s        child-pkg.ads
   --    Child.Pkg (child unit body)    child.pkg%b        child-pkg.adb
   --    Xyz.Arg.Lms (child subunit)    xyz.arg.lms%b      xyz-arg-lms.adb
   --    Accent?d (spec)                accentUc1d         accent?d.ads

   --  In the last example, ? stands for the graphic character that is
   --  displayed for the character UC_A_Acute (i.e. an upper case accented A).

   --  Note that the file name does *not* include the directory name. The
   --  management of directories is provided by Osint, and full file names
   --  are used only for error message purposes within GNAT itself.

   -------------------------
   -- File Name Crunching --
   -------------------------

   --  The rules described above give the file names that are generated if
   --  there is no restriction on the length of file names. However, the
   --  Get_File_Name routine will, if necessary according to the value in
   --  Opt.Maximum_File_Name_Length, crunch these file names down to this
   --  maximum value. For details of the crunching algorithm, see Krunch.

   ---------------------------------
   -- The Source_File_Name pragma --
   ---------------------------------

   --  The source file name pragma allows a program to override the normal
   --  naming convention. It is a configuration pragma, and so has the usual
   --  applicability of configuration pragmas (i.e. it applies to either an
   --  entire partition, or to all units in a compilation, or to a single
   --  unit, depending on how it is used. The form of the pragma is:

   --    pragma Source_File_Name (
   --      [UNIT_NAME =>] unit_NAME,
   --      [BODY_FILE_NAME | SPEC_FILE_NAME] => STRING_LITERAL)

   --  The given unit name is mapped to the given file name. The identifier
   --  for the second argument is required, and indicates whether this is
   --  the file name for the spec or for the body.

   --  If the given unit name is given to Get_File_Name, then the file name
   --  returned will be that established by the Source_File_Name pragma.

   -----------------
   -- Subprograms --
   -----------------

   type Expected_Unit_Type is (Expect_Body, Expect_Spec, Unknown);
   --  Return value from Get_Expected_Unit_Type

   function Get_Expected_Unit_Type
     (Fname : File_Name_Type)
      return  Expected_Unit_Type;
   --  If possible, determine whether the given file name corresponds to a unit
   --  that is a spec or body (e.g. by examining the extension). If this cannot
   --  be determined with the file naming conventions in use, then the returned
   --  value is set to Unknown.

   function Get_File_Name (Uname : Unit_Name_Type) return File_Name_Type;
   --  This function returns the file name that corresponds to a given unit
   --  name. The caller is responsible for ensuring that the unit name meets
   --  the requirements given in package Uname and described above.

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock tables before calling back end

   function Is_Internal_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True)
      return               Boolean;
   --  Similar to Is_Predefined_File_Name. The internal file set is a
   --  superset of the predefined file set including children of GNAT.

   function Is_Predefined_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True)
      return               Boolean;
   --  This function determines if the given file name (which must be a simple
   --  file name with no directory information) is the file name for one of
   --  the predefined library units. On return, Name_Buffer contains the
   --  file name. The Renamings_Included parameter indicates whether annex
   --  J renamings such as Text_IO are to be considered as predefined. If
   --  Renamings_Included is True, then Text_IO will return True, otherwise
   --  only children of Ada, Interfaces and System return True.

   function File_Name_Of_Spec (Name : Name_Id) return File_Name_Type;
   --  Returns the file name that corresponds to the spec of a given unit
   --  name. The unit name here is not encoded as a Unit_Name_Type, but is
   --  rather just a normal form name in lower case, e.g. "xyz.def".

   function File_Name_Of_Body (Name : Name_Id) return File_Name_Type;
   --  Returns the file name that corresponds to the body of a given unit
   --  name. The unit name here is not encoded as a Unit_Name_Type, but is
   --  rather just a normal form name in lower case, e.g. "xyz.def".

   procedure Set_File_Name (U : Unit_Name_Type; F : File_Name_Type);
   --  Make association between given unit name and given file name. This
   --  is the routine called to process a Source_File_Name pragma.

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using Tree_Read.
   --  Note that Initialize should not be called if Tree_Read is used.
   --  Tree_Read includes all necessary initialization.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using Tree_Write

end Fname;
