------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI  COMPONENTS                              --
--
--                         Exception Stack Spec                             --
--                               --
--                                                                          -
------------------------------------------------------------------------------

with CORBA;

package CORBA.Exceptions.Stack is

   procedure Get_Members
     (Exc_Occ : in CORBA.Exception_Occurrence;
      Exc_Mbr : out IDL_Exception_Members'Class);
   --  Find the member object associated to a given exception. Remove
   --  it from the stack and any other member whose corresponding
   --  exception was thrown after this one. Raise CORBA.Imp_Limit if
   --  the members are not in this stack.

   procedure Raise_Exception
     (Exc_Id  : in Ada.Exceptions.Exception_Id;
      Exc_Mbr : in IDL_Exception_Members'Class);
   pragma No_Return (Raise_Exception);
   --  Raise a CORBA exception associated to the member object
   --  Exc_Mbr. Store its members in a stack. If the stack size is
   --  bigger than stack_size, the oldest members are thrown away.

end  Corba.Exceptions.Stack;






