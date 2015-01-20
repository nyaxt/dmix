/*
 * GENERATED FILE - DO NOT EDIT
 * (c) Code Red Technologies Ltd, 2008-13
 * (c) NXP Semiconductors 2013-2015
 * Generated linker script file for LPC4370-M0
 * Created from generic_cpp.ld (LPCXpresso v7.5 (0 [Build 254] [2014-10-31] ))
 * By LPCXpresso v7.5.0 [Build 254] [2014-10-31]  on Tue Jan 20 17:25:45 JST 2015
 */


INCLUDE "dmix_lpc_m0_Debug_lib.ld"
INCLUDE "dmix_lpc_m0_Debug_mem.ld"

ENTRY(ResetISR)

SECTIONS
{

    /* MAIN TEXT SECTION */    
    .text : ALIGN(4)
    {
        FILL(0xff)
        __vectors_start__ = ABSOLUTE(.) ;
        KEEP(*(.isr_vector))
        
        /* Global Section Table */
        . = ALIGN(4) ;
        __section_table_start = .;
        __data_section_table = .;
        LONG(LOADADDR(.data));
        LONG(    ADDR(.data));
        LONG(  SIZEOF(.data));
        LONG(LOADADDR(.data_RAM2));
        LONG(    ADDR(.data_RAM2));
        LONG(  SIZEOF(.data_RAM2));
        LONG(LOADADDR(.data_RAM3));
        LONG(    ADDR(.data_RAM3));
        LONG(  SIZEOF(.data_RAM3));
        LONG(LOADADDR(.data_RAM4));
        LONG(    ADDR(.data_RAM4));
        LONG(  SIZEOF(.data_RAM4));
        __data_section_table_end = .;
        __bss_section_table = .;
        LONG(    ADDR(.bss));
        LONG(  SIZEOF(.bss));
        LONG(    ADDR(.bss_RAM2));
        LONG(  SIZEOF(.bss_RAM2));
        LONG(    ADDR(.bss_RAM3));
        LONG(  SIZEOF(.bss_RAM3));
        LONG(    ADDR(.bss_RAM4));
        LONG(  SIZEOF(.bss_RAM4));
        __bss_section_table_end = .;
        __section_table_end = . ;
        /* End of Global Section Table */
        

        *(.after_vectors*)
        
    } >Flash_1MB
    
    .text : ALIGN(4)    
    {
         *(.text*)
        *(.rodata .rodata.* .constdata .constdata.*)
        . = ALIGN(4);
        
        /* C++ constructors etc */
        . = ALIGN(4);
        KEEP(*(.init))
        
        . = ALIGN(4);
        __preinit_array_start = .;
        KEEP (*(.preinit_array))
        __preinit_array_end = .;
        
        . = ALIGN(4);
        __init_array_start = .;
        KEEP (*(SORT(.init_array.*)))
        KEEP (*(.init_array))
        __init_array_end = .;
        
        KEEP(*(.fini));
        
        . = ALIGN(4);
        KEEP (*crtbegin.o(.ctors))
        KEEP (*(EXCLUDE_FILE (*crtend.o) .ctors))
        KEEP (*(SORT(.ctors.*)))
        KEEP (*crtend.o(.ctors))
        
        . = ALIGN(4);
        KEEP (*crtbegin.o(.dtors))
        KEEP (*(EXCLUDE_FILE (*crtend.o) .dtors))
        KEEP (*(SORT(.dtors.*)))
        KEEP (*crtend.o(.dtors))
        . = ALIGN(4);
        /* End C++ */
    } > Flash_1MB

    /*
     * for exception handling/unwind - some Newlib functions (in common
     * with C++ and STDC++) use this. 
     */
    .ARM.extab : ALIGN(4)
    {
    	*(.ARM.extab* .gnu.linkonce.armextab.*)
    } > Flash_1MB
    __exidx_start = .;
    
    .ARM.exidx : ALIGN(4)
    {
    	*(.ARM.exidx* .gnu.linkonce.armexidx.*)
    } > Flash_1MB
    __exidx_end = .;
    
    _etext = .;
        
    
    /* DATA section for RamLoc72 */
    .data_RAM2 : ALIGN(4)
    {
       FILL(0xff)
       *(.ramfunc.$RAM2)
       *(.ramfunc.$RamLoc72)
    	*(.data.$RAM2*)
    	*(.data.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72 AT>Flash_1MB
    
    /* DATA section for RamM0Sub16 */
    .data_RAM3 : ALIGN(4)
    {
       FILL(0xff)
       *(.ramfunc.$RAM3)
       *(.ramfunc.$RamM0Sub16)
    	*(.data.$RAM3*)
    	*(.data.$RamM0Sub16*)
       . = ALIGN(4) ;
    } > RamM0Sub16 AT>Flash_1MB
    
    /* DATA section for RamM0Sub2 */
    .data_RAM4 : ALIGN(4)
    {
       FILL(0xff)
       *(.ramfunc.$RAM4)
       *(.ramfunc.$RamM0Sub2)
    	*(.data.$RAM4*)
    	*(.data.$RamM0Sub2*)
       . = ALIGN(4) ;
    } > RamM0Sub2 AT>Flash_1MB
    
    /* MAIN DATA SECTION */
    

    .uninit_RESERVED : ALIGN(4)
    {
        KEEP(*(.bss.$RESERVED*))
        . = ALIGN(4) ;
        _end_uninit_RESERVED = .;
    } > RamLoc128

	
	/* Main DATA section (RamLoc128) */
	.data : ALIGN(4)
	{
	   FILL(0xff)
	   _data = . ;
	   *(vtable)
	   *(.ramfunc*)
	   *(.data*)
	   . = ALIGN(4) ;
	   _edata = . ;
	} > RamLoc128 AT>Flash_1MB

    /* BSS section for RamLoc72 */
    .bss_RAM2 : ALIGN(4)
    {
    	*(.bss.$RAM2*)
    	*(.bss.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72
    /* BSS section for RamM0Sub16 */
    .bss_RAM3 : ALIGN(4)
    {
    	*(.bss.$RAM3*)
    	*(.bss.$RamM0Sub16*)
       . = ALIGN(4) ;
    } > RamM0Sub16
    /* BSS section for RamM0Sub2 */
    .bss_RAM4 : ALIGN(4)
    {
    	*(.bss.$RAM4*)
    	*(.bss.$RamM0Sub2*)
       . = ALIGN(4) ;
    } > RamM0Sub2

    /* MAIN BSS SECTION */
    .bss : ALIGN(4)
    {
        _bss = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4) ;
        _ebss = .;
        PROVIDE(end = .);
    } > RamLoc128
        
    /* NOINIT section for RamLoc72 */
    .noinit_RAM2 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM2*)
    	*(.noinit.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72 
    /* NOINIT section for RamM0Sub16 */
    .noinit_RAM3 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM3*)
    	*(.noinit.$RamM0Sub16*)
       . = ALIGN(4) ;
    } > RamM0Sub16 
    /* NOINIT section for RamM0Sub2 */
    .noinit_RAM4 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM4*)
    	*(.noinit.$RamM0Sub2*)
       . = ALIGN(4) ;
    } > RamM0Sub2 
    
    /* DEFAULT NOINIT SECTION */
    .noinit (NOLOAD): ALIGN(4)
    {
        _noinit = .;
        *(.noinit*) 
         . = ALIGN(4) ;
        _end_noinit = .;
    } > RamLoc128
    
    PROVIDE(_pvHeapStart = DEFINED(__user_heap_base) ? __user_heap_base : .);
    PROVIDE(_vStackTop = DEFINED(__user_stack_top) ? __user_stack_top : __top_RamLoc128 - 0);
}
