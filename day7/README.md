# Day 7

As these IntCode program problems get more complex I both regret and rejoice that I chose to implement them in C. On the plus side I don't have to worry about what data structure to use or how to represent the problem as it's all memory buffers and arrays. On the downside, it's all memory buffers and arrays!

Part 1 caught me out as at first I missed the requirement the phases were permutations of 0..4, not all 10,000 combinations of 5 digits. When I realised the test programs were not giving the right answers I then had to work out how to generate permutations with just memory buffers and arrays. The recursive solution worked out quite well:

* Make a big buffer to store all the permutations
* Start with the digits 0,1,2,3,4 and the notion that a certain number of digits are fixed in position.
* Rotate the non-fixed digits
* Recursively generate the permutations with one more fixed digit
* Copy the data into the output buffer
