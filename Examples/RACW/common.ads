package Common is
   pragma Pure;
   type Job is
      record
         Job_Duration : Integer;
      end record;
   type Worker is abstract tagged limited private;
   procedure Do_Job (W : access Worker; J : Job) is abstract;
private
   type Worker is abstract tagged limited null record;
end Common;
