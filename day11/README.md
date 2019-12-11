# Day 11

Ok, back to C for the IntCode computer. This one is interesting as my input/output buffers aren't going to work for interacting with the robot. I'm going to replace them with function calls that I'll wire into the robot. The robot will also keep a reference to the program so it can change the function that output calls each time, in a mini state-machine.

Since I'm in C I may as well store the current colour and whether a panel has been painted as separate bits in the same byte.

## Part 2

Part 2 is straightforward extension; I just needed a function to visualise the panel grid. Because it's quite large I first find the painted extent then only visualise that rectangle.
