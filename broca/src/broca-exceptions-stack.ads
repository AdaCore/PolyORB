-------------------------------------------------------------------
--   Description                                                 --
--   -----------                                                 --
--                                                               --
-------------------------------------------------------------------

with CORBA;
with Ada.Unchecked_Deallocation;

package Broca.Exceptions.Stack is

   --  the maximum number of exception members stored globally
   Pool_Size : constant Integer := 100;

   --  unique Ids are given to each exception members.
   --  A modular type is used to generate these unique Ids.
   --  This is the modulo of this modular type.
   Unique_Id_Modulo : constant Integer := 100000;

   --  This method finds the member object associated to a given exception.
   --  and removes from the stack this occurrence and any other member
   --  whose corresponding exception was thrown after this one.
   --  raises CORBA.Imp_Limit if the members are not in this stack
   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To : out IDL_Exception_Members'Class);

   --  This method raises a Corba exception associated to the member object
   --  Excp_Memb. It stored its members in a stack. If the stack size is
   --  bigger than stack_size, then the oldest members are thrown away
   procedure Raise_Exception (Excp : in Ada.Exceptions.Exception_Id;
                              Excp_Memb : in IDL_Exception_Members'Class);
   pragma No_Return (Raise_Exception);


private

   type IDL_Exception_Members_Ptr is
      access all IDL_Exception_Members'Class;

   procedure Free is
     new Ada.Unchecked_Deallocation (IDL_Exception_Members'Class,
                                     IDL_Exception_Members_Ptr);

   --  each exception occurrence is given a unique ID
   type Exception_Occurrence_ID is mod Unique_Id_Modulo;


   --  a structure to hold an ID an the corresponding member
   type Cell;
   type Cell_Ptr is access Cell;
   type Cell is
      record
         ID : Exception_Occurrence_ID := 0;
         Member_Ptr : IDL_Exception_Members_Ptr := null;
         Next : Cell_Ptr := null;
         Previous : Cell_Ptr := null;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_Ptr);

   --  The protected type for this package to be
   --  thread-safe
   protected The_Pool is

      --  puts an element in the list
      --  and throws away the oldest element if
      --  space is needed
      procedure Put (Excp_Mb : in IDL_Exception_Members_Ptr;
                     Excp_Id : in Exception_Occurrence_ID);

      --  gets an element from the list
      --  and free all the younger cells including
      --  the one returned
      procedure Get (From : in CORBA.Exception_Occurrence;
                     Result : out IDL_Exception_Members'Class);

      --  returns the next unique ID available
      procedure Get_Next_Id (Result : out Exception_Occurrence_ID);

      --  returns true if the stack is full
      function Is_Full return Boolean;

      --  removes the last element from the list
      procedure Remove_Last_Element;

   private
      Pool : Cell_Ptr := null;
      Next_Id : Exception_Occurrence_ID := 0;
      Current_Size : Integer := 0;
   end The_Pool;

end Broca.Exceptions.Stack;








