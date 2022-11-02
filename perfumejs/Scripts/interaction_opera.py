import time
from AndroidRunner.Device import Device

default_wait_time = 4

def tap(device: Device, x: int, y: int, sleep = 4) -> None:
    device.shell('input tap %s %s' % (x, y))
    # We need to wait for the display to update after the last click.
    # The time to update is vary. 
    time.sleep(sleep)


def main(device: Device, *args, **kwargs) -> None:
    # click on web element to measure FID and LCP
    tap(device,857, 656)

    # open a fresh tab to measure CLS
    tap(device, 756, 1848)
    
    
    tap(device, 529, 1784)