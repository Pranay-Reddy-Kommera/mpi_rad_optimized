# Makefile for KGEN-generated kernel

FC := ifort
#FC_FLAGS := -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source   -xHost  -O2
#FC_FLAGS := ${CPPDEFINES} -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source   -xHost  -O2
#CPPDEFINES := -DSPCVMCSW -DTAUMOLSW -DREFTRASW -DVRTQDR 
FC_FLAGS :=  -O3 -fopenmp -xHost -fp-model fast  -mkl -ipo -ip -align array64byte -align zcommons -align rec32byte -inline speed -override-limits -assume byterecl -assume realloc_lhs -xAVX -no-opt-dynamic-align
#FC_FLAGS :=  -O3 -xHost -fp-model fast -mkl -vec-report5 -align array64byte -align zcommons -align rec32byte -inline speed -override-limits -assume byterecl -assume realloc_lhs -xAVX -no-opt-dynamic-align
#FC_FLAGS := ${CPPDEFINES} -O3 -xHost -fp-model fast  -mkl -inline speed -xAVX 
#FC_FLAGS := ${CPPDEFINES} -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source   -xHost  -O2
ALL_OBJS := kernel_driver.o radsw.o kgen_utils.o rrsw_kg25.o rrsw_kg20.o rrtmg_sw_cldprmc.o rrsw_kg17.o rrtmg_sw_spcvmc.o rrsw_kg21.o rrsw_vsn.o rrtmg_sw_taumol.o rrsw_kg28.o rrsw_kg26.o shr_kind_mod.o rrsw_cld.o rrsw_kg27.o rrsw_kg18.o parrrsw.o rrsw_kg22.o rrsw_kg29.o rrsw_con.o rrsw_tbl.o rrsw_wvn.o rrtmg_sw_reftra.o rrsw_kg16.o rrtmg_sw_vrtqdr.o rrsw_kg24.o rrsw_kg23.o rrsw_kg19.o ppgrid.o rrtmg_sw_rad.o rrsw_ref.o rrtmg_sw_setcoef.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 radsw.o kgen_utils.o rrsw_kg25.o rrsw_kg20.o rrtmg_sw_cldprmc.o rrsw_kg17.o rrtmg_sw_spcvmc.o rrsw_kg21.o rrsw_vsn.o rrtmg_sw_taumol.o rrsw_kg28.o rrsw_kg26.o shr_kind_mod.o rrsw_cld.o rrsw_kg27.o rrsw_kg18.o parrrsw.o rrsw_kg22.o rrsw_kg29.o rrsw_con.o rrsw_tbl.o rrsw_wvn.o rrtmg_sw_reftra.o rrsw_kg16.o rrtmg_sw_vrtqdr.o rrsw_kg24.o rrsw_kg23.o rrsw_kg19.o ppgrid.o rrtmg_sw_rad.o rrsw_ref.o rrtmg_sw_setcoef.o
	${FC} ${FC_FLAGS} -c -o $@ $<

radsw.o: radsw.F90 kgen_utils.o rrtmg_sw_rad.o shr_kind_mod.o parrrsw.o ppgrid.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg25.o: rrsw_kg25.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg20.o: rrsw_kg20.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_cldprmc.o: rrtmg_sw_cldprmc.f90 kgen_utils.o shr_kind_mod.o parrrsw.o rrsw_vsn.o rrsw_wvn.o rrsw_cld.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg17.o: rrsw_kg17.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_spcvmc.o: rrtmg_sw_spcvmc.f90 kgen_utils.o shr_kind_mod.o parrrsw.o rrtmg_sw_taumol.o rrsw_wvn.o rrsw_tbl.o rrtmg_sw_reftra.o rrtmg_sw_vrtqdr.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg21.o: rrsw_kg21.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_vsn.o: rrsw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_taumol.o: rrtmg_sw_taumol.f90 kgen_utils.o shr_kind_mod.o rrsw_vsn.o rrsw_kg16.o rrsw_con.o rrsw_wvn.o parrrsw.o rrsw_kg17.o rrsw_kg18.o rrsw_kg19.o rrsw_kg20.o rrsw_kg21.o rrsw_kg22.o rrsw_kg23.o rrsw_kg24.o rrsw_kg25.o rrsw_kg26.o rrsw_kg27.o rrsw_kg28.o rrsw_kg29.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg28.o: rrsw_kg28.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg26.o: rrsw_kg26.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_cld.o: rrsw_cld.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg27.o: rrsw_kg27.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg18.o: rrsw_kg18.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrsw.o: parrrsw.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg22.o: rrsw_kg22.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg29.o: rrsw_kg29.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_con.o: rrsw_con.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_tbl.o: rrsw_tbl.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_wvn.o: rrsw_wvn.f90 kgen_utils.o parrrsw.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_reftra.o: rrtmg_sw_reftra.f90 kgen_utils.o shr_kind_mod.o rrsw_vsn.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg16.o: rrsw_kg16.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_vrtqdr.o: rrtmg_sw_vrtqdr.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg24.o: rrsw_kg24.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg23.o: rrsw_kg23.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_kg19.o: rrsw_kg19.f90 kgen_utils.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_rad.o: rrtmg_sw_rad.f90 kgen_utils.o shr_kind_mod.o parrrsw.o rrsw_con.o rrtmg_sw_cldprmc.o rrtmg_sw_setcoef.o rrtmg_sw_spcvmc.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_ref.o: rrsw_ref.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_setcoef.o: rrtmg_sw_setcoef.f90 kgen_utils.o shr_kind_mod.o rrsw_ref.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
