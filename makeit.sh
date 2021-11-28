PROJECTNAME=serial
#PROJECTNAME=HDMI_test
#PROJECTNAME=HDMI_test_DDR # This one uses DDR primitives for higher freq, ready for higher res (to be tested)
VERILOGS="simple_dualport_ram_8k_lattice.v"
VHDLS="serial.vhd quadflash_cache.vhd"

if [ $1 == "clean" ]; then
  rm -f *.bit *.json *.config *.svf *~
  exit
fi

yosys -p "ghdl --std=08 -fsynopsys $VHDLS -e; synth_ecp5 -abc9 -top serial -json $PROJECTNAME.json" $VERILOGS || exit
nextpnr-ecp5 --force --timing-allow-fail --json $PROJECTNAME.json --lpf ulx3s_v20.lpf --textcfg $PROJECTNAME.config --85k --freq 25 --package CABGA381 || exit
ecppack --compress --svf-rowsize 100000 --svf $PROJECTNAME.svf $PROJECTNAME.config $PROJECTNAME.bit || exit
fujprog-v48-mac-x64 $PROJECTNAME.bit || exit
# To flash permanently, use instead:
#   Use ujprog -j FLASH $PROJECTNAME.bit 