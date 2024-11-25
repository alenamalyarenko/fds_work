#define plot_3d_out
#undef plot_hrr_out
#define output_nc

#define write_init
#define output_full_variable

#define coupled

#if defined coupled
# define global_mesh
# define init_file_in
# define init_t_in
# define init_u_in
# define coupled_bc_file
# define coupled_bc 
#endif
