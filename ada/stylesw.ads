------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T Y L E S W                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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

--  This package contains the style switches used for setting style options.
--  The only clients of this package are the body of Style and the body of
--  Switches. All other style checking issues are handled using the public
--  interfaces in the spec of Style.

with Types; use Types;

package Stylesw is

   --------------------------
   -- Style Check Switches --
   --------------------------

   --  These flags are used to control the details of the style checking
   --  options. The default values shown here correspond to no style
   --  checking. If any of these values is set to a non-default value,
   --  then Opt.Style_Check is set True to active calls to this package.

   --  The actual mechanism for setting these switches to other than
   --  default values is via the Set_Style_Check_Option procedure or
   --  through a call to Set_Default_Style_Check_Options. They should
   --  not be set directly in any other manner.

   Style_Check_Attribute_Casing : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatya switches. If
   --  it is True, then attribute names (including keywords such as
   --  digits used as attribute names) must be in mixed case.

   Style_Check_Blanks_At_End : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyb switches. If
   --  it is True, then spaces at the end of lines are not permitted.

   Style_Check_Comments : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyc switches. If
   --  it is True, then comments are style checked as follows:
   --
   --    All comments must be at the start of the line, or the first
   --    minus must be preceded by at least one space.
   --
   --    For a comment that is not at the start of a line, the only
   --    requirement is that a space follow the comment characters.
   --
   --    For a coment that is at the start of the line, one of the
   --    following conditions must hold:
   --
   --      The comment characters are the only non-blank characters on the line
   --
   --      The comment characters are followed by an exclamation point (the
   --      sequence --! is used by gnatprep for marking deleted lines).
   --
   --      The comment characters are followed by two space characters
   --
   --      The line consists entirely of minus signs
   --
   --      The comment characters are followed by a single space, and the
   --      last two characters on the line are also comment characters.
   --
   --  Note: the reason for the last two conditions is to allow "boxed"
   --  comments where only a single space separates the comment characters.

   Style_Check_End_Labels : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatye switches. If
   --  it is True, then optional END labels must always be present.

   Style_Check_Form_Feeds : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyf switches. If
   --  it is True, then form feeds and vertical tabs are not allowed in
   --  the source text.

   Style_Check_Horizontal_Tabs : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyh switches. If
   --  it is True, then horizontal tabs are not allowed in source text.

   Style_Check_If_Then_Layout : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyi switches. If
   --  it is True, then a THEN keyword may not appear on the line that
   --  immediately follows the line containing the corresponding IF.
   --
   --  This permits one of two styles for IF-THEN layout. Either the
   --  IF and THEN keywords are on the same line, where the condition
   --  is short enough, or the conditions are continued over to the
   --  lines following the IF and the THEN stands on its own. For
   --  example:
   --
   --    if X > Y then
   --
   --    if X > Y
   --      and then Y < Z
   --    then
   --
   --  are allowed, but
   --
   --    if X > Y
   --    then
   --
   --  is not allowed.

   Style_Check_Indentation : Column_Number range 0 .. 9 := 0;
   --  This can be set non-zero by using the -gnatg or -gnatyn (n a digit)
   --  switches. If it is non-zero it activates indentation checking with
   --  the indicated indentation value. A value of zero turns off checking.
   --  The requirement is that any new statement, line comment, declaration
   --  or keyword such as END, start on a column that is a multiple of the
   --  indentiation value.

   Style_Check_Keyword_Casing : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyk switches. If
   --  it is True, then keywords are required to be in all lower case.
   --  This rule does not apply to keywords such as digits appearing as
   --  an attribute name.

   Style_Check_Max_Line_Length : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatym switches. If
   --  it is True, it activates checking for a maximum line length of 79
   --  characters (chosen to fit in standard 80 column displays that don't
   --  handle the limiting case of 80 characters cleanly).

   Style_Check_Pragma_Casing : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyp switches. If
   --  it is True, then pragma names must use mixed case.

   Style_Check_Layout : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyl switches. If
   --  it is True, it activates checks that constructs are indented as
   --  suggested by the examples in the RM syntax, e.g. that the ELSE
   --  keyword must line up with the IF keyword.

   Style_Check_References : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyr switches. If
   --  it is True, then all references to declared identifiers are
   --  checked. The requirement is that casing of the reference be the
   --  same as the casing of the corresponding declaration.

   Style_Check_Specs : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatys switches. If
   --  it is True, then separate specs are required to be present for
   --  all procedures except parameterless library level procedures.
   --  The exception means that typical main programs do not require
   --  separate specs.

   Style_Check_Tokens : Boolean := False;
   --  This can be set True by using the -gnatg or -gnatyt switches. If
   --  it is True, then the style check that requires canonical spacing
   --  between various punctuation tokens as follows:
   --
   --    ABS and NOT must be followed by a space
   --
   --    => must be surrounded by spaces
   --
   --    <> must be preceded by a space or left paren
   --
   --    Binary operators other than ** must be surrounded by spaces.
   --    There is no restriction on the layout of the ** binary operator.
   --
   --    Colon must be surrounded by spaces
   --
   --    Colon-equal (assignment) must be surrounded by spaces
   --
   --    Comma must be the first non-blank character on the line, or be
   --    immediately preceded by a non-blank character, and must be followed
   --    by a blank.
   --
   --    A space must precede a left paren following a digit or letter,
   --    and a right paren must not be followed by a space (it can be
   --    at the end of the line).
   --
   --    A right paren must either be the first non-blank character on
   --    a line, or it must be preceded by a non-blank character.
   --
   --    A semicolon must not be preceded by a blank, and must not be
   --    followed by a non-blank character.
   --
   --    A unary plus or minus may not be followed by a space
   --
   --    A vertical bar must be surrounded by spaces
   --
   --  Note that a requirement that a token be preceded by a space is
   --  met by placing the token at the start of the line, and similarly
   --  a requirement that a token be followed by a space is met by
   --  placing the token at the end of the line. Note that in the case
   --  where horizontal tabs are permitted, a horizontal tab is acceptable
   --  for meeting the requirement for a space.

   Style_Max_Line_Length : Int := 79;
   --  Value used to check maximum line length. Can be reset by a call to
   --  Set_Max_Line_Length. The value here is the default if no such call.

   -----------------
   -- Subprograms --
   -----------------

   procedure Set_Default_Style_Check_Options;
   --  This procedure is called to set the default style checking options
   --  in response to a -gnatg switch or -gnaty with no suboptions.

   procedure Set_Style_Check_Option (C : Character; OK : out Boolean);
   --  This procedure is called to set the style checking option that
   --  corresponds to a single character in the -gnatyxxx string. If
   --  the option character is valid, then the appropriate internal
   --  checking switch is set, and OK is set True on return. If the
   --  option character is invalid, then OK is set False on return.

   procedure Set_Max_Line_Length (Max : Nat);
   --  This procedure has the opportunity of setting a new value for the
   --  maximum line length which is different from the standard default
   --  value of 79 used for this check. This call also implies setting
   --  the max line length check active.

end Stylesw;
