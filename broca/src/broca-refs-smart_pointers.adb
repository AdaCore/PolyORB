with Ada.Unchecked_Deallocation;
with Broca.Soft_Links; use Broca.Soft_Links;

package body Broca.Refs.Smart_Pointers is

   function Create return Smart_Pointer is
      CA : Smart_Pointer;
   begin
      CA.A := new T;
      CA.A.Counter := 1;
      return CA;
   end Create;

   function Deref
     (CA : in Smart_Pointer)
     return Unchecked_Reference is
   begin
      return Unchecked_Reference (CA.A);
   end Deref;

   procedure Free is new Ada.Unchecked_Deallocation (T, Any);

   procedure Initialize (CA : in out Smart_Pointer) is
   begin
      CA := (Ada.Finalization.Controlled with A => null);
   end Initialize;

   procedure Adjust (CA : in out Smart_Pointer) is
   begin
      Enter_Critical_Section;
      if CA.A /= null then
         CA.A.Counter := Natural'Succ (CA.A.Counter);
      end if;
      Leave_Critical_Section;
   end Adjust;

   procedure Finalize (CA : in out Smart_Pointer) is
   begin
      if CA.A /= null then
         CA.A.Counter := Natural'Pred (CA.A.Counter);
         if CA.A.Counter = 0 then
            Free (CA.A);
         end if;
      end if;
   end Finalize;

end Broca.Refs.Smart_Pointers;
