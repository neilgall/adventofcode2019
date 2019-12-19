# Day 17

IntCode again. I'm reusing my IntCode server from Day 15, but communicating with it in Python this time.

Part 1 is pretty easy - receive the image and scan it for intersections.

Part 2 was hard, very hard. At first I tried to keep the route as a list of model objects but the processing was just too slow in Python. Rendering to a string and working on it using string processing was less ideal but much faster.
