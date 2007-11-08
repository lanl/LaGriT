      logical isafield,isgrad_wt,isdiscrete
      real*8 merge_length_4d, refine_length_4d, toldamage_4d,
     * field_scale_factor,toldamage_discrete,pc_refine,error_refine
      character*32 adaption_field_name,surfcmo_name
      common /massager/ merge_length_4d, refine_length_4d, toldamage_4d,
     * field_scale_factor,toldamage_discrete,pc_refine,error_refine
      common /massagel/ isafield,isgrad_wt,isdiscrete
      common /massagec/ adaption_field_name,surfcmo_name
      save /massager/, /massagel/, /massagec/
c 
c these variables are set in by the mode command
c
