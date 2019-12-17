import socket

def receive_image(s):
    image = ""
    while True:
        r = s.recv(1024)
        if r:
            for b in r:
                image += '%c' % (b)
        else:
            break
    return image.strip().split("\n")


def find_intersections(image):
    for y in range(1, len(image)-1):
        line = image[y]
        for x in range(1, len(line)-1):
            if image[y][x] == '#':
                if image[y-1][x] == '#' and image [y+1][x] == '#' \
                    and image[y][x-1] == '#' and image[y][x+1] == '#':
                        yield (x,y)


def find_alignment_parameters(image):
    for x,y in find_intersections(image):
        yield x * y


def part1(image):
    total = sum(find_alignment_parameters(image))
    print(f"Part 1 .. {total}")


if __name__ == "__main__":
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect(("localhost", 5000))
        image = receive_image(s)
        part1(image)
