#include "board.h"

const uint32_t OscRateIn = 12000000;
const uint32_t ExtRateIn = 0;

#if defined(CORE_M4)
/* Structure for initial base clock states */
struct CLK_BASE_STATES {
	CHIP_CGU_BASE_CLK_T clk;	/* Base clock */
	CHIP_CGU_CLKIN_T clkin;	/* Base clock source, see UM for allowable souorces per base clock */
	bool autoblock_enab;/* Set to true to enable autoblocking on frequency change */
	bool powerdn;		/* Set to true if the base clock is initially powered down */
};

/* Initial base clock states are mostly on */
STATIC const struct CLK_BASE_STATES InitClkStates[] = {
	{CLK_BASE_PHY_TX, CLKIN_ENET_TX, true, false},
#if defined(USE_RMII)
	{CLK_BASE_PHY_RX, CLKIN_ENET_TX, true, false},
#else
	{CLK_BASE_PHY_RX, CLKIN_ENET_RX, true, false},
#endif

	/* Clocks derived from dividers */
	{CLK_BASE_LCD, CLKIN_IDIVC, true, false},
	{CLK_BASE_USB1, CLKIN_IDIVD, true, true}
};

/* SPIFI high speed pin mode setup */
STATIC const PINMUX_GRP_T spifipinmuxing[] = {
	{0x3, 3,  (SCU_PINIO_FAST | SCU_MODE_FUNC3)},	/* SPIFI CLK */
	{0x3, 4,  (SCU_PINIO_FAST | SCU_MODE_FUNC3)},	/* SPIFI D3 */
	{0x3, 5,  (SCU_PINIO_FAST | SCU_MODE_FUNC3)},	/* SPIFI D2 */
	{0x3, 6,  (SCU_PINIO_FAST | SCU_MODE_FUNC3)},	/* SPIFI D1 */
	{0x3, 7,  (SCU_PINIO_FAST | SCU_MODE_FUNC3)},	/* SPIFI D0 */
	{0x3, 8,  (SCU_PINIO_FAST | SCU_MODE_FUNC3)}	/* SPIFI CS/SSEL */
};

STATIC const PINMUX_GRP_T pinmuxing[] = {
	// SSP0
	/* SSEL: P1.0: [Serial Expansion Interface] */
	{0x1, 0, (SCU_PINIO_FAST | SCU_MODE_FUNC5)},
	/* MISO: P1.1: [Serial Expansion Interface] */
	{0x1, 1, (SCU_PINIO_FAST | SCU_MODE_FUNC5)},
	/* MOSI: P1.2: [Serial Expansion Interface] */
	{0x1, 2, (SCU_PINIO_FAST | SCU_MODE_FUNC5)},
	/* SCLK: P3.0: [Serial Expansion Interface] */
	{0x3, 0, (SCU_PINIO_FAST | SCU_MODE_FUNC4)},

	// SSP1
	/* SSEL: P1.5: [Serial Expansion Interface] */
	{0x1, 5, (SCU_PINIO_FAST | SCU_MODE_FUNC5)},
	/* MISO: P1.3: [Serial Expansion Interface] */
	{0x1, 3, (SCU_PINIO_FAST | SCU_MODE_FUNC5)},
	/* MOSI: P1.4: [Serial Expansion Interface] */
	{0x1, 4, (SCU_PINIO_FAST | SCU_MODE_FUNC5)},
	/* SCLK: PF.4: [Serial Expansion Interface] */
	{0xF, 4, (SCU_PINIO_FAST | SCU_MODE_FUNC0)},
};

/* Sets up system pin muxing */
static void Board_SetupMuxing(void)
{
	/* Setup system level pin muxing */
	Chip_SCU_SetPinMuxing(pinmuxing, sizeof(pinmuxing) / sizeof(PINMUX_GRP_T));
}

/* Set up and initialize clocking prior to call to main */
static void Board_SetupClocking(void)
{
	Chip_SetupCoreClock(CLKIN_CRYSTAL, MAX_CLOCK_FREQ, true);

	/* Setup system base clocks and initial states. This won't enable and
	   disable individual clocks, but sets up the base clock sources for
	   each individual peripheral clock. */
	for (unsigned i = 0; i < (sizeof(InitClkStates) / sizeof(InitClkStates[0])); i++) {
		Chip_Clock_SetBaseClock(InitClkStates[i].clk, InitClkStates[i].clkin,
								InitClkStates[i].autoblock_enab, InitClkStates[i].powerdn);
	}

	/* Reset and enable 32Khz oscillator */
	LPC_CREG->CREG0 &= ~((1 << 3) | (1 << 2));
	LPC_CREG->CREG0 |= (1 << 1) | (1 << 0);

	/* SPIFI pin setup is done prior to setting up system clocking */
	Chip_SCU_SetPinMuxing(spifipinmuxing, sizeof(spifipinmuxing) / sizeof(PINMUX_GRP_T));

	/* Setup a divider E for main PLL clock switch SPIFI clock to that divider.
	   Divide rate is based on CPU speed and speed of SPI FLASH part. */
#if (MAX_CLOCK_FREQ > 180000000)
	Chip_Clock_SetDivider(CLK_IDIV_E, CLKIN_MAINPLL, 5);
#else
	Chip_Clock_SetDivider(CLK_IDIV_E, CLKIN_MAINPLL, 4);
#endif
	Chip_Clock_SetBaseClock(CLK_BASE_SPIFI, CLKIN_IDIVE, true, false);

	/* Attach main PLL clock to divider C with a divider of 2 */
	Chip_Clock_SetDivider(CLK_IDIV_C, CLKIN_MAINPLL, 2);
}

/* Set up and initialize hardware prior to call to main */
void Board_SystemInit(void)
{
#if defined(CORE_M4)
	/* Setup system clocking and memory. This is done early to allow the
	   application and tools to clear memory and use scatter loading to
	   external memory. */
	Board_SetupMuxing();
	Board_SetupClocking();
#endif /* defined(CORE_M4) */
}

#endif /* defined(CORE_M4) */
