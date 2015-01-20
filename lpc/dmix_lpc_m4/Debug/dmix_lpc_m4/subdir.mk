################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../dmix_lpc_m4/cr_cpp_config.cpp \
../dmix_lpc_m4/cr_startup_lpc43xx.cpp 

C_SRCS += \
../dmix_lpc_m4/crp.c \
../dmix_lpc_m4/dmix_lpc.c \
../dmix_lpc_m4/libusbdev.c \
../dmix_lpc_m4/libusbdev_desc.c \
../dmix_lpc_m4/sysinit.c 

OBJS += \
./dmix_lpc_m4/cr_cpp_config.o \
./dmix_lpc_m4/cr_startup_lpc43xx.o \
./dmix_lpc_m4/crp.o \
./dmix_lpc_m4/dmix_lpc.o \
./dmix_lpc_m4/libusbdev.o \
./dmix_lpc_m4/libusbdev_desc.o \
./dmix_lpc_m4/sysinit.o 

C_DEPS += \
./dmix_lpc_m4/crp.d \
./dmix_lpc_m4/dmix_lpc.d \
./dmix_lpc_m4/libusbdev.d \
./dmix_lpc_m4/libusbdev_desc.d \
./dmix_lpc_m4/sysinit.d 

CPP_DEPS += \
./dmix_lpc_m4/cr_cpp_config.d \
./dmix_lpc_m4/cr_startup_lpc43xx.d 


# Each subdirectory must supply rules for building sources it contributes
dmix_lpc_m4/%.o: ../dmix_lpc_m4/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: MCU C++ Compiler'
	arm-none-eabi-c++ -D__MULTICORE_NONE -DDEBUG -DCORE_M4 -D__USE_CMSIS_DSPLIB=CMSIS_DSPLIB_CM4 -DCPP_USE_HEAP -D__LPC43XX__ -D__USE_LPCOPEN -D__CODE_RED -D__NEWLIB__ -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_board_nxp_lpclink2_4370\inc" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_chip_43xx\inc" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_chip_43xx\inc\usbd" -O0 -g3 -Wall -c -fmessage-length=0 -fno-builtin -ffunction-sections -fdata-sections -fno-rtti -fno-exceptions -fsingle-precision-constant -mcpu=cortex-m4 -mfpu=fpv4-sp-d16 -mfloat-abi=softfp -mthumb -D__NEWLIB__ -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.o)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

dmix_lpc_m4/%.o: ../dmix_lpc_m4/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: MCU C Compiler'
	arm-none-eabi-gcc -D__MULTICORE_NONE -DDEBUG -D__CODE_RED -DCORE_M4 -D__USE_CMSIS_DSPLIB=CMSIS_DSPLIB_CM4 -DCPP_USE_HEAP -D__LPC43XX__ -D__USE_LPCOPEN -D__NEWLIB__ -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_board_nxp_lpclink2_4370\inc" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_chip_43xx\inc" -I"C:\Users\kouhei\Documents\GitHub\dmix\lpc\dmix_lpc_m4\dmix_lpc_m4\lpc_chip_43xx\inc\usbd" -O0 -g3 -Wall -c -fmessage-length=0 -fno-builtin -ffunction-sections -fdata-sections -fsingle-precision-constant -mcpu=cortex-m4 -mfpu=fpv4-sp-d16 -mfloat-abi=softfp -mthumb -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.o)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


