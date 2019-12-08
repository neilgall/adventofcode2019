# Day 8

Let's go for Kotlin today. I like this language and use it daily professionally but I really should check out Kotlin/Native as the JVM is a terrible place for little small bits of code like this. The simplest build system I could devise was a shell script to compile the Kotlin source to a jar and run it on the JVM. Yes it's all taken care of for you in IntelliJ but a language that needs a heavyweight IDE just to use is only half-baked in my opinion.

So on to the problem. Part 1 is pretty simple
* chunk the input into 25x6 character chunks
* find the one with the least zeros - this is a `minBy` operation
* for that chunk (layer), count the `1` and `2` characters and multiply

Part 2 involves flattening the layers into a single layer, with a per-pixel flattening function. Adding a fully transparent layer on top this is a simple fold over all the layers, where merging two layers is done by merging each pair of overlapping pixels.
