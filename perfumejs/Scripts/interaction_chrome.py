import time
from AndroidRunner.Device import Device

default_wait_time = 4

def tap(device: Device, x: int, y: int, sleep = 4) -> None:
    device.shell('input tap %s %s' % (x, y))
    # We need to wait for the display to update after the last click.
    # The time to update is vary. 
    time.sleep(sleep)


def main(device: Device, *args, **kwargs) -> None:
    # Do task 1
    # click on web element to measure FID and LCP
    tap(device,864, 668)

    # open a fresh tab to measure CLS
    tap(device, 880, 144)
    
    
    tap(device, 87, 168)