------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . E X C E P T I O N S . S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with Broca.Debug;

package body Broca.Exceptions.Stack is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.exceptions.stack");
   procedure O is new Broca.Debug.Output (Flag);

   ---------------------
   -- Raise_Exception --
   ---------------------
   procedure Raise_Exception (Excp : in Ada.Exceptions.Exception_Id;
                              Excp_Memb : in IDL_Exception_Members'Class) is
      ID : Exception_Occurrence_ID;
      Ex_Mb : IDL_Exception_Members_Ptr
        := new IDL_Exception_Members'Class' (Excp_Memb);
   begin
      The_Pool.Get_Next_Id (ID);
      The_Pool.Put (Ex_Mb, ID);
      Ada.Exceptions.Raise_Exception (Excp,
                                      Exception_Occurrence_ID'Image (ID));
      --  should never reach this point
      raise Program_Error;
   end Raise_Exception;

   -----------------
   -- Get_Members --
   -----------------
   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To : out IDL_Exception_Members'Class) is
   begin
      The_Pool.Get (From, To);
   end Get_Members;



   --------------------
   -- The_Pool body --
   --------------------

   protected body The_Pool is

      --------------
      --  Is_Full --
      --------------
      function Is_Full return Boolean is
      begin
         return (Current_Size = Pool_Size);
      end Is_Full;


      ----------
      --  Put --
      ----------
      procedure Put (Excp_Mb : in IDL_Exception_Members_Ptr;
                     Excp_Id : in Exception_Occurrence_ID) is
         Head : Cell_Ptr;
      begin
         pragma Debug (O ("Put starts Excp_ID="
                          & Exception_Occurrence_ID'Image (Excp_Id)
                          & " ,Current_Size="
                          & Integer'Image (Current_Size)));
         if Is_Full then
            Remove_Last_Element;
         end if;

         Head := new Cell' (ID => Excp_Id,
                            Member_Ptr => Excp_Mb,
                            Next => Pool,
                            Previous => null);
         if Pool /= null then
            Pool.all.Previous := Head;
         end if;

         Pool := Head;

         Current_Size := Current_Size + 1;

         pragma Debug (O ("Put ends Excp_ID="
                          & Exception_Occurrence_ID'Image (Excp_Id)
                          & " ,Current_Size="
                          & Integer'Image (Current_Size)));
      end Put;


      -----------
      --  Get  --
      -----------
      procedure Get (From : in CORBA.Exception_Occurrence;
                     Result : out IDL_Exception_Members'Class) is
         Index : Cell_Ptr := Pool;
         Excp_Id : Exception_Occurrence_ID
           := Exception_Occurrence_ID'Value
           (Ada.Exceptions.Exception_Message (From));
      begin

         pragma Debug (O ("Get starts Excp_ID="
                          & Exception_Occurrence_ID'Image (Excp_Id)
                          & " ,Current_Size="
                          & Integer'Image (Current_Size)));

         if Index = null then
            Broca.Exceptions.Raise_Internal;
         end if;

         --  if it is the first
         if Pool.all.ID = Excp_Id then
            Pool := Pool.all.Next;

         else
            --  else find it
            while (Index /= null)
              and then Index.all.ID /= Excp_Id loop
               Index := Index.all.Next;
            end loop;

            --  if not found :
            --  either it is a bug
            --  or the pool size has been reached
            if Index = null then
               Broca.Exceptions.Raise_Imp_Limit;

            else
               Index.Previous.all.Next := Index.all.Next;
               if Index.Next /= null then
                  Index.Next.all.Previous := Index.Previous;
               end if;
            end if;
         end if;

         Result := Index.all.Member_Ptr.all;

         --  free the resources
         Free (Index.all.Member_Ptr);
         Free (Index);
         Current_Size := Current_Size - 1;

         pragma Debug (O ("Get ends Excp_ID="
                          & Exception_Occurrence_ID'Image (Excp_Id)
                          & " ,Current_Size="
                          & Integer'Image (Current_Size)));
      end Get;


      ------------------
      --  Get_Next_Id --
      ------------------
      procedure Get_Next_Id (Result : out Exception_Occurrence_ID) is
      begin
         Next_Id := Next_Id + 1;
         Result := Next_Id;
      end Get_Next_Id;

      --------------------------
      --  Remove_Last_Element --
      --------------------------
      procedure Remove_Last_Element is
         Index : Cell_Ptr := Pool;
      begin
         if Index = null then
            Broca.Exceptions.Raise_Internal;
         end if;

         while Index.all.Next /= null loop
            Index := Index.all.Next;
         end loop;

         Index.Previous.all.Next := null;
         Free (Index.all.Member_Ptr);
         Free (Index);
         Current_Size := Current_Size - 1;
      end Remove_Last_Element;


   end The_Pool;

end Broca.Exceptions.Stack;
