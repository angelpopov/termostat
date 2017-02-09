modprobe usbserial
modprobe ftdi_sio
digitemp_DS9097U -c ~/.digitemprc -a -q | cut -d: -f4
