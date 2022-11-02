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
    tap(device,965, 156)

    # click ____
    tap(device,965, 156)
    
    tap(device,965, 156)
    
    tap(device,965, 156)
    
    tap(device,796, 1844)
    
    tap(device,297, 1808)
    
    tap(device,867, 1824)
    
    tap(device,958, 1840)
    
    tap(device,992, 828)
    
    tap(device,81, 156)
    
    tap(device,54, 120)
    
    #tap(device, 168, 1784)


