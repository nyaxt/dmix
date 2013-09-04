#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/sleep.h>
#include <util/delay.h>
#include <string.h>
#include <stdlib.h>

#ifdef SIMAVR
#include <avr/avr_mcu_section.h>
AVR_MCU(F_CPU, MCU);
#endif // SIMAVR

#ifndef sbi
#define sbi(PORT,BIT) PORT|=_BV(BIT)
#define cbi(PORT,BIT) PORT&=~_BV(BIT)
#endif

volatile char g_uart_buffer[256];
volatile unsigned char g_uart_buffer_read = 0;
volatile unsigned char g_uart_buffer_write = 0;
volatile unsigned char g_uart_recved_lines = 0;
volatile unsigned char g_uart_read_lines = 0;

#define BAUD 57600

void uart_putc(char c);

ISR(USART_RX_vect)
{
  char r = UDR0;
  if(r == '\r')
  {
    r = '\n';
  }

  g_uart_buffer[g_uart_buffer_write] = r;
  ++ g_uart_buffer_write;

  if(r == '\n')
  {
    ++ g_uart_recved_lines;
  }

  uart_putc(r);
}

void
uart_putc(char c)
{
  if(c == '\n')
  {
    uart_putc('\r');
  }

  loop_until_bit_is_set(UCSR0A, UDRE0);
  UDR0 = c;
}

void
uart_puts(const char* s)
{
  while(*s) { uart_putc(*s++); }
}

#define UART_DATA_NREAD (g_uart_buffer_write - g_uart_buffer_read)

inline
char
uart_peekchar(void)
{
  return g_uart_buffer[g_uart_buffer_read];
}

inline
char
uart_getc(void)
{
  char r = uart_peekchar();
  ++ g_uart_buffer_read;
  return r;
}

#define UART_LINE_NREAD (g_uart_recved_lines - g_uart_read_lines)

void
uart_gets(char* buf)
{
  while((*buf++ = uart_getc()) != '\n')
    ;
  *buf = '\0';
  ++ g_uart_read_lines;
}

static void hardwareInit(void)
{
  // PORTB
  PORTB = 0x00;
  DDRB = 0xff;

  // PORTC
  PORTC = 0x00;
  DDRC = 0xff;

  // PORTD
  PORTD = 0x00;
  DDRD = 0xfe;

  // UART
  UCSR0C = _BV(UCSZ01) | _BV(UCSZ00); /* 8bit */
  #include <util/setbaud.h>
  UBRR0H = UBRRH_VALUE;
  UBRR0L = UBRRL_VALUE;
  #if USE_2X
  UCSR0A |= (1 << U2X0);
  #else
  UCSR0A &= ~(1 << U2X0);
  #endif

  UCSR0B = _BV(RXEN0) | _BV(TXEN0) | _BV(RXCIE0); /* rx/tx enable && receive by interrupt */
}

const char*
my_itoa(int i)
{
  static char buf[8];
  return itoa(i, buf, 10);
}

void
cmd_set_vol(int ch, int val)
{
  uart_puts("VOL ");
  uart_puts(my_itoa(ch));
  uart_puts(", ");
  uart_puts(my_itoa(val));
  uart_puts("\n");
}

void
cmd_get_vol(int ch)
{
  uart_puts("VOL ");
  uart_puts(my_itoa(ch));
  uart_puts("\n");
}

void
handle_uart_cmd(void)
{
  while(UART_LINE_NREAD)
  {
    static const char* DELIMITER = " \n";
    char line[64]; memset(line, 0, sizeof(line));
    uart_gets(line);

    char* save;
    char* cmd = strtok_r(line, DELIMITER, &save);
#define NEXT_TOKEN strtok_r(NULL, DELIMITER, &save)
    if(strcmp(cmd, "VOL") == 0)
    {
      char* chstr = NEXT_TOKEN;
      char* valstr = NEXT_TOKEN;

      int ch = atoi(chstr);
      int val = atoi(valstr);
      cmd_set_vol(ch, val);
    }
    else if(strcmp(cmd, "VOL?") == 0)
    {
      char* chstr = NEXT_TOKEN;

      int ch = atoi(chstr);
      cmd_get_vol(ch);
    }
#ifdef SIMAVR
    else if(strcmp(cmd, "HALT") == 0)
    {
      sleep_cpu(); 
    }
#endif
    else
    {
      uart_puts("unknown cmd\n"); 
    }
  }
}

int main(void)
{
  // wdt_enable(WDTO_2S);
  hardwareInit();

  sei();
  uart_puts("hello!\n");
  for(;;)
  {
    // wdt_reset();

    handle_uart_cmd();
  }
  
  return 0;
}
