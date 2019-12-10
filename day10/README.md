# Day 10

So we're comparing a set of items with each other. Normally that's an O(N²) algorithm at best. But here we have to do those N² comparisons for every item in the list, making it O(N³). Surely we can do better than that?

I scribbled on my [reMarkable](https://remarkable.com) tablet all the way to work on the bus, and came up with this algorithm:

	for each candidate C
		sort the remaining asteroids by distance from the candidate
		for each remaining asteroid A
			get the offset from C to A
			if this is vertical, the step is 0,1 or 0,-1 depending on direction
			else divide the offset by the GCD of offset-x and offset-y
			trace away from A to the edge of space by the calculated step
				eliminate any asteroids seen at the step locations

Seems to work, and my Python solution runs in 1.3 seconds.
