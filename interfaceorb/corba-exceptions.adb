------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     C O R B A . E X C E P T I O N S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.20 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

--  This package is a sub package of package corba dealing with Corba
--  exceptions.  It provides two main functions : Raise_corba_Exception and
--  Get_Members. These functions allows the programmer to associate to each
--  exception a "member" structure with all kinds of datas he needs.

with Ada.Unchecked_Deallocation;
with Ada.Tags;

with AdaBroker; use AdaBroker;
with AdaBroker.Exceptions;
pragma Warnings (Off, AdaBroker.Exceptions);
with AdaBroker.Constants;
with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

package body CORBA.Exceptions is

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("corba-exceptions");
   procedure O is new AdaBroker.Debug.Output (Flag);

   use type Constants.Exception_Id;

   type ID_Num is mod 65000;
   ID_Number : ID_Num := 0;
   --  Number of exceptions raised until now used to build an identifier
   --  for each exception

   type IDL_Exception_Members_Ptr is access all IDL_Exception_Members'Class;
   procedure Free is new Ada.Unchecked_Deallocation
     (IDL_Exception_Members'Class, IDL_Exception_Members_Ptr);

   type Cell;
   type Cell_Ptr is access all Cell;
   type Cell (N : Positive) is
      record
         Value : IDL_Exception_Members_Ptr;
         ID    : Standard.String (1 .. N);
         Next  : Cell_Ptr;
      end record;
   --  Definition of type list of IDL_Exception_Members in order to store
   --  the different member object waiting for their associated exception
   --  to be catched.  Actually, this list works as a stack since the last
   --  exception raised may be the first catched.  Each member is
   --  associated to a string which references it and allows the procedure
   --  Get_Members to find it again since the corresponding exception will
   --  be raised with the same string as message.  Actually, the string is
   --  the image of ID_Number that is incremented each time an exception is
   --  raised.

   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_Ptr);

   List : Cell_Ptr := null;

   procedure Get
     (From   : in Ada.Exceptions.Exception_Occurrence;
      Result : out IDL_Exception_Members'Class);

   procedure Put
     (V    : in IDL_Exception_Members'Class;
      ID_V : in Standard.String);

   ---------
   -- Put --
   ---------

   procedure Put
     (V    : in IDL_Exception_Members'Class;
      ID_V : in Standard.String)
   is
      Tmp : Cell_Ptr;
   begin
      pragma Debug (O ("put member type : " & Ada.Tags.External_Tag (V'Tag)));
      Tmp := new Cell'(N     => ID_V'Length,
                       Value => new IDL_Exception_Members'Class'(V),
                       ID    => ID_V,
                       Next  => List);
      List := Tmp;
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From   : in Ada.Exceptions.Exception_Occurrence;
      Result : out IDL_Exception_Members'Class)
   is
      Tmp : Cell_Ptr := List;
      Old : Cell_Ptr := null;
      ID  : Standard.String := Ada.Exceptions.Exception_Message (From);
   begin
      if Tmp = null then
         --  Raise an Ada Exception AdaBroker_Fatal_Error
         pragma Debug (O ("get **** tmp = null ****"));

         Ada.Exceptions.Raise_Exception
           (AdaBroker_Fatal_Error'Identity,
            "cannot translate member associated to " &
            Ada.Exceptions.Exception_Name (From));

      else
         loop
            pragma Debug (O ("get : enter loop"));

            if Tmp.all.ID = ID then
               declare
                  Member : IDL_Exception_Members'Class := Tmp.all.Value.all;
               begin
                  pragma Debug (O ("get : find correct member"));
                  --  We can suppress the correponding cell
                  if Old = null then
                     List := Tmp.all.Next;
                  else
                     Old.all.Next := Tmp.all.Next;
                  end if;

                  pragma Debug (O ("get : free memory"));

                  Free (Tmp.all.Value);
                  Free (Tmp);

                  pragma Debug (O ("result type " &
                                   Ada.Tags.External_Tag (Result'Tag)));

                  pragma Debug (O ("member type " &
                                   Ada.Tags.External_Tag (Member'Tag)));

                  --  At last, return the result
                  Result := Member;

                  pragma Debug (O ("get : leave loop"));
                  return;
               end;

            else
               --  If the end of list is reached
               if Tmp.all.Next = null then

               --  Raise an Ada Exception AdaBroker_Fatal_Error
                  pragma Debug (O ("get : cannot find member"));

                  Ada.Exceptions.Raise_Exception
                    (AdaBroker_Fatal_Error'Identity,
                     "cannot translate member associated to " &
                     Ada.Exceptions.Exception_Name (From));

               else
                  --  Else go to the next element of the list
                  Old := Tmp;
                  Tmp := Tmp.Next;
               end if;
            end if;
         end loop;
      end if;
   end Get;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out IDL_Exception_Members'Class)
   is
   begin
      pragma Debug (O ("get_member : enter"));
      Get (From, To);
      pragma Debug (O ("get_member : leave"));
   end Get_Members;

   ---------------------------
   -- Raise_CORBA_exception --
   ---------------------------

   procedure Raise_CORBA_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in IDL_Exception_Members'Class)
   is
      ID : Standard.String := ID_Num'Image (ID_Number);
   begin
      --  Stores the member object Member_List.
      Put (Excp_Memb, ID);

      --  Raises the Ada exception with the ID String as message
      Ada.Exceptions.Raise_Exception (Excp, ID);
   end Raise_CORBA_Exception;

end CORBA.Exceptions;
