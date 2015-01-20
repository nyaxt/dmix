################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/board.c \
../dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/board_sysinit.c 

OBJS += \
./dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/board.o \
./dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/board_sysinit.o 

C_DEPS += \
./dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/board.d \
./dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/board_sysinit.d 


# Each subdirectory must supply rules for building sources it contributes
dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/%.o: ../dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: MCU C Compiler'
	arm-none-eabi-gcc -D__MULTICORE_NONE -DDEBUG -D__CODE_RED -DCORE_M4 -D__USE_CMSIS_DSPLIB=CMSIS_DSPLIB_CM4 -DCPP_USE_HEAP -D__LPC43XX__ -D__USE_LPCOPEN -D__NEWLIB__ -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_board_nxp_lpclink2_4370\inc" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_chip_43xx\inc" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_chip_43xx\inc\usbd" -O0 -g3 -Wall -c -fmessage-length=0 -fno-builtin -ffunction-sections -fdata-sections -fsingle-precision-constant -mcpu=cortex-m4 -mfpu=fpv4-sp-d16 -mfloat-abi=softfp -mthumb -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.o)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


