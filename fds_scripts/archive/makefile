FC = ftn
NCFLAGS = -I${NETCDF_DIR}/include
NCLIBS = -L${NETCDF_DIR}/lib -L${NETCDF_DIR}/lib64 -lnetcdf -lnetcdff

SRCS = read.f90
OBJS = $(SRCS:.f90=.o)
EXEC = read.exe

all: $(EXEC)

$(EXEC): $(SRCS)
	$(FC)  -o $@ $< $(NCFLAGS) $(NCLIBS)

%.o: %.F
	$(FC) $(NCFLAGS) $(NCLIBS)  -o $@ $<

clean:
	rm -f *.o *.mod



