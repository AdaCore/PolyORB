with Tapes;
package Name_Server is
   pragma Remote_Call_Interface;

   -- Dynamic binding to remote operations is achieved
   -- using the access-to-limited-class-wide type Tape_Ptr

   type Tape_Ptr is access all Tapes.Tape'Class;

   -- The following statically bound remote operations
   -- allow for a name-server capability in this example

   function  Find     (Name : String) return Tape_Ptr;
   procedure Register (Name : in String; T : in Tape_Ptr);
   procedure Remove   (T : in Tape_Ptr);
   function  Name     (T : in Tape_Ptr) return String;

   -- More operations

end Name_Server;
