with Broca.Exceptions;

package body Corba is
   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
     renames Broca.Exceptions.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InvalidName_Members) is
   begin
      To := InvalidName_Members'(IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InconsistentTypeCode_Members) is
   begin
      To := InconsistentTypeCode_Members'
        (IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence; To : out PolicyError_Members)
   is
   begin
      To := PolicyError_Members'(IDL_Exception_Members with null record);
   end Get_Members;

   function To_CORBA_String
     (S : in Standard.String)
      return CORBA.String is
   begin
      return CORBA.String (Ada.Strings.Unbounded.To_Unbounded_String (S));
   end To_CORBA_String;

   function To_Standard_String
     (S : in CORBA.String)
      return Standard.String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (S));
   end To_Standard_String;

--    function Length
--      (S : in CORBA.String)
--       return CORBA.Unsigned_Long is
--    begin
--       return CORBA.Unsigned_Long
--         (Ada.Strings.Unbounded.Length
--          (Ada.Strings.Unbounded.Unbounded_String (S)));
--    end Length;
end Corba;

