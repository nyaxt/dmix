cd C:\nxp\LPCXpresso_7.5.0_254\lpcxpresso\bin
crt_emu_cm_redlink -flash-load-exec "C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m0\Debug\dmix_lpc_m0.axf" -g -2  -vendor=NXP -pLPC4370-M0 -flash-driver=LPC18_43_SPIFI_1MB_4KB.cfx -x C:/Users/kouhei/Documents/GitHub/dmix/lpc/dmix_lpc_m0/Debug
pause > nul
