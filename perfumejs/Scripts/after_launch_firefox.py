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
    # click ____
    tap(device,610, 1756)

    # click ____
    tap(device, 141,1788)
    
    
    #tap(device, 168, 1784)

