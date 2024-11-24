#undef plot_3d_out
#undef plot_hrr_out
#define output_nc

#define coupled

#if defined 
# define global_mesh
# define init_file_in
# define init_t_in
# define init_u_in
# undef coupled_bc

#endif
