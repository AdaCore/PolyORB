package SP is

   pragma Shared_Passive;

   Shared_Integer : Integer;

   type Shared_Record_T is record
      I : Integer;
      B : Boolean;
   end record;

   Shared_Record : Shared_Record_T;
end SP;


