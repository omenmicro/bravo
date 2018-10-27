# OMEN BRAVO

As-easy-as-possible, but still expandable single board computer with 65C02 CPU.

[![I sell on Tindie](https://d2ss6ovg47m0r5.cloudfront.net/badges/tindie-larges.png)](https://www.tindie.com/stores/parallaxis/?ref=offsite_badges&utm_source=sellers_parallaxis&utm_medium=badges&utm_campaign=badge_large)

### Issue 3 problem fix

Issue 3 board has some design problems, which is fixed in the Issue 4. Issue 3 is fully working, but you have to manual solder:

- A wire between GND port and a pin 8 of the 74HC138.
- A 180k resistor parallel to the Q1 (between the CPU pins 3 and 37).
- A 47pF capacitor between the CPU pins 1 and 3

![Issue 3 fixes](hw/bravo-issue3-fix.png)