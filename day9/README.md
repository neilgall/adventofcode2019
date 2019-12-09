# Day 9

Well that was tough! The new requirements revealed a few deficiencies with my IntCode emulator. My addressing modes were a mess for writes, but somehow made it this far anyway. The need to expand the memory on demand meant a substantial refactor. I also had to use a nonstandard (but widely supported) `__int128` data type to support the big numbers required.
