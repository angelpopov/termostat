modprobe usbserial
modprobe ftdi_sio
digitemp_DS9097U -c ~/.digitemprc -a -q | tee -a temps.txt | cut -d: -f4  | cat
