/*
 * @brief USB Band Width and WCID example descriptors.
 *
 * @note
 * Copyright(C) NXP Semiconductors, 2013
 * All rights reserved.
 *
 * @par
 * Software that is described herein is for illustrative purposes only
 * which provides customers with programming information regarding the
 * LPC products.  This software is supplied "AS IS" without any warranties of
 * any kind, and NXP Semiconductors and its licensor disclaim any and
 * all warranties, express or implied, including all implied warranties of
 * merchantability, fitness for a particular purpose and non-infringement of
 * intellectual property rights.  NXP Semiconductors assumes no responsibility
 * or liability for the use of the software, conveys no license or rights under any
 * patent, copyright, mask work right, or any other intellectual property rights in
 * or to any products. NXP Semiconductors reserves the right to make changes
 * in the software without notification. NXP Semiconductors also makes no
 * representation or warranty that such application will be suitable for the
 * specified use without further testing or modification.
 *
 * @par
 * Permission to use, copy, modify, and distribute this software and its
 * documentation is hereby granted, under NXP Semiconductors' and its
 * licensor's relevant copyrights in the software, without fee, provided that it
 * is used in conjunction with NXP Semiconductors microcontrollers.  This
 * copyright, permission, and disclaimer notice must appear in all copies of
 * this code.
 */

#include "../dmix_lpc_m4/app_usbd_cfg.h"

/*****************************************************************************
 * Private types/enumerations/variables
 ****************************************************************************/

/*****************************************************************************
 * Public types/enumerations/variables
 ****************************************************************************/

/**
 * USB Standard Device Descriptor
 */
ALIGNED(4) const uint8_t USB_DeviceDescriptor[] = {
	USB_DEVICE_DESC_SIZE,				/* bLength */
	USB_DEVICE_DESCRIPTOR_TYPE,			/* bDescriptorType */
	WBVAL(0x0200),						/* bcdUSB: 2.00 */
	0x00,								/* bDeviceClass */
	0x00,								/* bDeviceSubClass */
	0x00,								/* bDeviceProtocol */
	USB_MAX_PACKET0,					/* bMaxPacketSize0 */
	WBVAL(0xF055),						/* idVendor */
	WBVAL(LUSB_PID),					/* idProduct */
	WBVAL(0x0100),						/* bcdDevice: 1.00 */
	0x01,								/* iManufacturer */
	0x02,								/* iProduct */
	0x03,								/* iSerialNumber */
	0x01								/* bNumConfigurations */
};

/**
 * USB Device Qualifier
 */
ALIGNED(4) const uint8_t USB_DeviceQualifier[] = {
	USB_DEVICE_QUALI_SIZE,					/* bLength */
	USB_DEVICE_QUALIFIER_DESCRIPTOR_TYPE,	/* bDescriptorType */
	WBVAL(0x0200),							/* bcdUSB: 2.00 */
	0x00,									/* bDeviceClass */
	0x00,									/* bDeviceSubClass */
	0x00,									/* bDeviceProtocol */
	USB_MAX_PACKET0,						/* bMaxPacketSize0 */
	0x01,									/* bNumOtherSpeedConfigurations */
	0x00									/* bReserved */
};

/* USB FSConfiguration Descriptor */
/*   All Descriptors (Configuration, Interface, Endpoint, Class, Vendor */
ALIGNED(4) uint8_t USB_FsConfigDescriptor[] = {
	/* Configuration 1 */
	USB_CONFIGURATION_DESC_SIZE,		/* bLength */
	USB_CONFIGURATION_DESCRIPTOR_TYPE,	/* bDescriptorType */
	WBVAL(							/* wTotalLength */
		USB_CONFIGURATION_DESC_SIZE +
		USB_INTERFACE_DESC_SIZE     +
		3 * USB_ENDPOINT_DESC_SIZE
		),
	0x01,							/* bNumInterfaces */
	0x01,							/* bConfigurationValue */
	0x00,							/* iConfiguration */
	USB_CONFIG_SELF_POWERED,		/* bmAttributes */
	USB_CONFIG_POWER_MA(100),		/* bMaxPower */
	/* Interface 0, Alternate Setting 0, Custom Class */
	USB_INTERFACE_DESC_SIZE,		/* bLength */
	USB_INTERFACE_DESCRIPTOR_TYPE,	/* bDescriptorType */
	0x00,							/* bInterfaceNumber */
	0x00,							/* bAlternateSetting */
	0x03,							/* bNumEndpoints */
	0xFF,							/* bInterfaceClass */
	0xF0,							/* bInterfaceSubClass */
	0x00,							/* bInterfaceProtocol */
	0x04,							/* iInterface */
	/* Bulk Out Endpoint */
	USB_ENDPOINT_DESC_SIZE,			/* bLength */
	USB_ENDPOINT_DESCRIPTOR_TYPE,	/* bDescriptorType */
	LUSB_OUT_EP,					/* bEndpointAddress */
	USB_ENDPOINT_TYPE_BULK,			/* bmAttributes */
	WBVAL(64),						/* wMaxPacketSize */
	0,								/* bInterval */
	/* Bulk In Endpoint */
	USB_ENDPOINT_DESC_SIZE,			/* bLength */
	USB_ENDPOINT_DESCRIPTOR_TYPE,	/* bDescriptorType */
	LUSB_IN_EP,						/* bEndpointAddress */
	USB_ENDPOINT_TYPE_BULK,			/* bmAttributes */
	WBVAL(64),						/* wMaxPacketSize */
	0,								/* bInterval */
	/* INT In Endpoint */
	USB_ENDPOINT_DESC_SIZE,			/* bLength */
	USB_ENDPOINT_DESCRIPTOR_TYPE,	/* bDescriptorType */
	LUSB_INT_EP,					/* bEndpointAddress */
	USB_ENDPOINT_TYPE_INTERRUPT,	/* bmAttributes */
	WBVAL(4),						/* wMaxPacketSize */
	0x8,							/* bInterval: 8 ms */
	/* Terminator */
	0								/* bLength */
};

/* USB HSConfiguration Descriptor */
/*   All Descriptors (Configuration, Interface, Endpoint, Class, Vendor */
ALIGNED(4) uint8_t USB_HsConfigDescriptor[] = {
	/* Configuration 1 */
	USB_CONFIGURATION_DESC_SIZE,		/* bLength */
	USB_CONFIGURATION_DESCRIPTOR_TYPE,	/* bDescriptorType */
	WBVAL(							/* wTotalLength */
		USB_CONFIGURATION_DESC_SIZE +
		USB_INTERFACE_DESC_SIZE     +
		3 * USB_ENDPOINT_DESC_SIZE
		),
	0x01,							/* bNumInterfaces */
	0x01,							/* bConfigurationValue */
	0x00,							/* iConfiguration */
	USB_CONFIG_SELF_POWERED,		/* bmAttributes */
	USB_CONFIG_POWER_MA(100),		/* bMaxPower */
	/* Interface 0, Alternate Setting 0, Custom Class */
	USB_INTERFACE_DESC_SIZE,		/* bLength */
	USB_INTERFACE_DESCRIPTOR_TYPE,	/* bDescriptorType */
	0x00,							/* bInterfaceNumber */
	0x00,							/* bAlternateSetting */
	0x03,							/* bNumEndpoints */
	0xFF,							/* bInterfaceClass */
	0xF0,							/* bInterfaceSubClass */
	0x00,							/* bInterfaceProtocol */
	0x04,							/* iInterface */
	/* EP1 Bulk In Endpoint */
	USB_ENDPOINT_DESC_SIZE,			/* bLength */
	USB_ENDPOINT_DESCRIPTOR_TYPE,	/* bDescriptorType */
	LUSB_OUT_EP,					/* bEndpointAddress */
	USB_ENDPOINT_TYPE_BULK,			/* bmAttributes */
	WBVAL(512),						/* wMaxPacketSize */
	0,								/* bInterval */
	/* EP1 Bulk In Endpoint */
	USB_ENDPOINT_DESC_SIZE,			/* bLength */
	USB_ENDPOINT_DESCRIPTOR_TYPE,	/* bDescriptorType */
	LUSB_IN_EP,						/* bEndpointAddress */
	USB_ENDPOINT_TYPE_BULK,			/* bmAttributes */
	WBVAL(512),						/* wMaxPacketSize */
	0,								/* bInterval */
	/* EP2 INT In Endpoint */
	USB_ENDPOINT_DESC_SIZE,			/* bLength */
	USB_ENDPOINT_DESCRIPTOR_TYPE,	/* bDescriptorType */
	LUSB_INT_EP,					/* bEndpointAddress */
	USB_ENDPOINT_TYPE_INTERRUPT,	/* bmAttributes */
	WBVAL(4),						/* wMaxPacketSize */
	0x9,							/* bInterval : 8ms */
	/* Terminator */
	0								/* bLength */
};

/* USB String Descriptor (optional) */
ALIGNED(4) const uint8_t USB_StringDescriptor[] = {
	/* Index 0x00: LANGID Codes */
	0x04,							/* bLength */
	USB_STRING_DESCRIPTOR_TYPE,		/* bDescriptorType */
	WBVAL(0x0409),					/* wLANGID  0x0409 = US English*/
	/* Index 0x01: Manufacturer */
	(13 * 2 + 2),					/* bLength (13 Char + Type + length) */
	USB_STRING_DESCRIPTOR_TYPE,		/* bDescriptorType */
	'n', 0,
	'y', 0,
	'a', 0,
	'x', 0,
	't', 0,
	's', 0,
	't', 0,
	'e', 0,
	'p', 0,
	'.', 0,
	'c', 0,
	'o', 0,
	'm', 0,
	/* Index 0x02: Product */
	(8 * 2 + 2),					/* bLength (19 Char + Type + length) */
	USB_STRING_DESCRIPTOR_TYPE,		/* bDescriptorType */
	'd', 0,
	'm', 0,
	'i', 0,
	'x', 0,
	'_', 0,
	'l', 0,
	'p', 0,
	'c', 0,
	/* Index 0x03: Serial Number */
	(2 * 2 + 2),					/* bLength (13 Char + Type + length) */
	USB_STRING_DESCRIPTOR_TYPE,		/* bDescriptorType */
	'V', 0,
	'0', 0,
	/* Index 0x04: Interface 0, Alternate Setting 0 */
	(6 * 2 + 2),					/* bLength (6 Char + Type + length) */
	USB_STRING_DESCRIPTOR_TYPE,		/* bDescriptorType */
	'S', 0,
	't', 0,
	'r', 0,
	'e', 0,
	'a', 0,
	'm', 0,
};
