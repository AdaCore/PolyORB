with CORBA.Exceptions;

package body CORBA is

   ----------------
   -- GetMembers --
   ----------------

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out Ex_Body)
   is
   begin
      --  Calls the correponding procedure in CORBA.Exception
      CORBA.Exceptions.Get_Members (From, To);
   end Get_Members;

   ---------------------------
   -- Raise_CORBA_Exception --
   ---------------------------

   procedure Raise_CORBA_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in IDL_Exception_Members'Class)
   is
   begin
      --  Calls the correponding procedure in CORBA.Exception
      CORBA.Exceptions.Raise_CORBA_Exception (Excp, Excp_Memb);
   end Raise_CORBA_Exception;

   ---------------------
   -- To_CORBA_String --
   ---------------------

   function To_CORBA_String
     (S : in Standard.String)
      return CORBA.String is
   begin
      return CORBA.String (Ada.Strings.Unbounded.To_Unbounded_String (S));
   end To_CORBA_String;

   ------------------------
   -- To_Standard_String --
   ------------------------

   function To_Standard_String
     (S : in CORBA.String)
      return Standard.String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (S));
   end To_Standard_String;

   ---------------------
   -- To_CORBA_String --
   ---------------------

   function To_CORBA_String
     (S : in Constants.Exception_Id)
      return CORBA.String
   is
   begin
      return CORBA.String
        (Ada.Strings.Unbounded.To_Unbounded_String
         (Standard.String (S)));
   end To_CORBA_String;

   ---------------------
   -- To_Exception_Id --
   ---------------------

   function To_Exception_Id
     (S : in CORBA.String)
      return Constants.Exception_Id
   is
   begin
      return Constants.Exception_Id (To_Standard_String (S));
   end To_Exception_Id;

   ------------
   -- Length --
   ------------

   function Length
     (S : in CORBA.String)
      return CORBA.Unsigned_Long
   is
   begin
      return CORBA.Unsigned_Long
        (Ada.Strings.Unbounded.Length
         (Ada.Strings.Unbounded.Unbounded_String (S)));
   end Length;

end CORBA;
