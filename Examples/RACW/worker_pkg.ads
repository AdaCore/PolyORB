with Common; use Common;
package Worker_Pkg is
   type Real_Worker is new Worker with
     record
        Speed : Integer;
     end record;
   procedure Do_Job (W : access Real_Worker; J : Job);
end Worker_Pkg;
