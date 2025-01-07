# nesmon
A port of Woz Monitor for the NES/Famicom based on the code and documation from https://www.sbprojects.net/projects/apple1/wozmon.php

Joypad controls have been **removed**. A fork that uses Joypad for input can be found at [https://github.com/TakuikaNinja/nesmon](https://github.com/TakuikaNinja/nesmon).

Input is now done through either a Family Basic Keyboard or using a compatible Keyboard Mouse Host interface (like pico-usbfamikb)

![](images/example.png)

An imperfect version of the Test Program can be run by entering the following code

**Important**

The location of the ECHO subroutine in memory has changed. In the 25-01-06 builds it was at 8318 (18 83), now it is at 8352 (52 83). The location in memory may change again so always check and only use the example code with the most recent builds.
```
0: A9 b 0 b AA b 20 b 52 b 83 b E8 b 8A (RET)
8: 4C b 2 b 0 (RET)
0 (RET)
R (RET)
```
b means blank or space; and (RET) hit the "return" key on the keyboard

It doesn't align properly due to window width and missing characters, but it works.