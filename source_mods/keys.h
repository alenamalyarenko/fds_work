#define coupled

#if defined coupled
# define global_mesh
# define init_file_in
# define init_t_in
# define init_u_in
# define output_nc

# undef coupled_bc

# define plot_3d_out
# undef plot_hrr_out
#endif
