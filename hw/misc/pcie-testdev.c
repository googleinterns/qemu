/*
 * QEMU PCI Express test device
 *
 * Copyright (c) 2020
 * Author: Jennifer Liu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "hw/pci/pci.h"
#include "hw/qdev-properties.h"
#include "qemu/log.h"
#include "sysemu/kvm.h"
#include "trace.h"
#include <inttypes.h>

#define VENDOR_ID_GOOGLE 0x1ae0
#define SUBSYSTEM_ID 0xbe
#define PCI_EXPRESS_DEVICE_ID_TEST 0x6b
#define TYPE_PCIE_TEST_DEV "pcie-testdev"

#define SRCADDR 0x00
#define DSTADDR 0x08
#define SIZE 0x10
#define DOORBELL 0x18
#define STATUS 0x20
#define BUSY_DELAY 0x28 /* delay in ms to simulate a longer transfer */
#define AGGRESSIVE 0x30

typedef struct DMARegs {
    dma_addr_t srcaddr;
    dma_addr_t dstaddr;
    dma_addr_t size;
    dma_addr_t doorbell;
    dma_addr_t status;
    dma_addr_t busy_delay;
} DMARegs;

typedef struct PCIeTestDevState {
    PCIDevice parent_obj;
    MemoryRegion membar;
    DMARegs regs;
    bool aggressive;
    dma_addr_t aggression_delay; /* delay between aggressive DMA transfers */
} PCIeTestDevState;

#define PCI_EXPRESS_TEST_DEV(obj) \
    OBJECT_CHECK(PCIeTestDevState, (obj), TYPE_PCIE_TEST_DEV)

static Property props[] = {
    DEFINE_PROP_BOOL("aggressive", PCIeTestDevState, aggressive, false),
    DEFINE_PROP_UINT64("aggression_delay_ms", PCIeTestDevState,
                       aggression_delay, false),
    DEFINE_PROP_UINT64("srcaddr", PCIeTestDevState, regs.srcaddr, false),
    DEFINE_PROP_UINT64("dstaddr", PCIeTestDevState, regs.dstaddr, false),
    DEFINE_PROP_UINT64("size", PCIeTestDevState, regs.size, false),
    DEFINE_PROP_UINT64("busy_delay_ms", PCIeTestDevState, regs.busy_delay,
                       false),
    DEFINE_PROP_END_OF_LIST()
};

static uint64_t pcie_testdev_read(void *opaque, hwaddr addr, unsigned size)
{
    PCIeTestDevState *s = opaque;

    switch (addr) {
    case SRCADDR:
        return s->regs.srcaddr;
        break;
    case DSTADDR:
        return s->regs.dstaddr;
        break;
    case SIZE:
        return s->regs.size;
        break;
    case STATUS:
        return s->regs.status;
        break;
    case BUSY_DELAY:
        return s->regs.busy_delay;
    case AGGRESSIVE:
        return s->aggressive;
    default:
        return 0x0;
    }
}

static void enable_bus_master(PCIeTestDevState *s)
{
    pci_set_word(s->parent_obj.config + PCI_COMMAND,
                 s->parent_obj.config[PCI_COMMAND] | PCI_COMMAND_MASTER);
}

static void dma_transfer(PCIeTestDevState *s)
{
    uint16_t *buf = g_malloc0(s->regs.size);
    int transfer_status = 1;
    if (pci_dma_read(&s->parent_obj, s->regs.srcaddr, buf, s->regs.size) == 0) {
        if (pci_dma_write(&s->parent_obj, s->regs.dstaddr, buf, s->regs.size) ==
            0) {
            transfer_status = 0;
        }
    }
    trace_dma_transfer((void *)s->regs.srcaddr, (void *)s->regs.dstaddr,
                       s->regs.size, transfer_status);
    g_free(buf);
    s->regs.status = 0x0;
}

static void aggressive_dma_transfer(PCIeTestDevState *s)
{
    s->regs.status = 0x1;
    dma_transfer(s);

    if (s->aggressive) {
        QEMUTimer *dma_timer = timer_new_ms(QEMU_CLOCK_VIRTUAL,
                                            (void *)aggressive_dma_transfer, s);
        timer_mod(dma_timer,
                  qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL) + s->aggression_delay);
    }
}

static void pcie_testdev_write(void *opaque, hwaddr addr, uint64_t val,
                               unsigned size)
{
    PCIeTestDevState *s = opaque;

    switch (addr) {
    case SRCADDR:
        s->regs.srcaddr = val;
        break;
    case DSTADDR:
        s->regs.dstaddr = val;
        break;
    case SIZE:
        s->regs.size = val;
        break;
    case DOORBELL:
        s->regs.doorbell = val;
        if ((val & 0x1) && s->regs.size) {
            if (s->regs.status != 0) {
                qemu_log_mask(LOG_GUEST_ERROR,
                              "Tried to start DMA on busy device. "
                              "Source: %p, Dest: %p, Size: %" PRIu64 "\n",
                              (void *)s->regs.srcaddr, (void *)s->regs.dstaddr,
                              s->regs.size);
            } else {
                s->regs.status = 0x1;

                /* Use a timer to simulate delay in device making a DMA_transfer
                 */
                QEMUTimer *timer =
                    timer_new_ms(QEMU_CLOCK_VIRTUAL, (void *)dma_transfer, s);
                timer_mod(timer, qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL) +
                                     s->regs.busy_delay);
            }
        }
        break;
    case BUSY_DELAY:
        s->regs.busy_delay = val;
        break;
    case AGGRESSIVE:
        if ((val & 0x1) == !s->aggressive) {
            if (s->aggressive == 0x0) {
                s->aggressive = 0x1;
                aggressive_dma_transfer(s);
            } else {
                s->aggressive = 0x0;
            }
        }
    default:
        break;
    }
}

static const MemoryRegionOps pcie_testdev_io_ops = {
    .read = pcie_testdev_read,
    .write = pcie_testdev_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
};

static void pcie_testdev_realize(PCIDevice *pcie_dev, Error **errp)
{
    PCIeTestDevState *s = PCI_EXPRESS_TEST_DEV(pcie_dev);

    memory_region_init_io(&s->membar, OBJECT(s), &pcie_testdev_io_ops, s,
                          "pci-testdev-membar", 65536);
    pci_register_bar(pcie_dev, 0,
                     PCI_BASE_ADDRESS_SPACE_MEMORY |
                         PCI_BASE_ADDRESS_MEM_PREFETCH |
                         PCI_BASE_ADDRESS_MEM_TYPE_64,
                     &s->membar);

    pcie_endpoint_cap_init(pcie_dev, 0);

    /* Aggressor mode: Self-enable bus mastering and start DMA immediately */
    if (s->aggressive) {
        QEMUTimer *bus_master_timer =
            timer_new_ms(QEMU_CLOCK_VIRTUAL, (void *)enable_bus_master, s);
        timer_mod(bus_master_timer, qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL));
        QEMUTimer *dma_timer = timer_new_ms(QEMU_CLOCK_VIRTUAL,
                                            (void *)aggressive_dma_transfer, s);
        timer_mod(dma_timer, qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL));
    }
}

static void pcie_testdev_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    PCIDeviceClass *k = PCI_DEVICE_CLASS(klass);

    k->device_id = PCI_EXPRESS_DEVICE_ID_TEST;
    k->vendor_id = VENDOR_ID_GOOGLE;
    k->subsystem_id = SUBSYSTEM_ID;
    k->subsystem_vendor_id = VENDOR_ID_GOOGLE;
    k->class_id = PCI_CLASS_OTHERS;
    k->realize = pcie_testdev_realize;
    dc->desc = "PCIe Test Device";
    device_class_set_props(dc, props);
}

static const TypeInfo pcie_testdev_info = {
    .name = TYPE_PCIE_TEST_DEV,
    .parent = TYPE_PCI_DEVICE,
    .class_init = pcie_testdev_class_init,
    .instance_size = sizeof(PCIeTestDevState),
    .interfaces =
        (InterfaceInfo[]){
            { INTERFACE_PCIE_DEVICE },
            {},
        },
};

static void pcie_testdev_register_types(void)
{
    type_register_static(&pcie_testdev_info);
}

type_init(pcie_testdev_register_types)
