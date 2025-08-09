#define write_init

#undef plot_3d_out
#undef plot_hrr_out
#undef output_nc


#undef coupled
#undef vent_debug
#undef coupled_debug

#if defined coupled
# define global_mesh
# define init_file_in
# define init_t_in
# define init_u_in
# define coupled_bc_file
# define coupled_bc 
# define atm_variables
#endif


