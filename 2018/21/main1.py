
# Source:
#
#     https://www.reddit.com/r/adventofcode/comments/a86jgt/comment/ec8lyck/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

def run_activation_system(magic_number, is_part_1):
    seen = set()
    c = 0
    last_unique_c = -1

    while True:

        a = c | 65536
        c = magic_number

        while True:
            c = (((c + (a & 255)) & 16777215) * 65899) & 16777215

            if 256 > a:
                if is_part_1:
                    return c
                else:
                    if c not in seen:
                        seen.add(c)
                        last_unique_c = c
                        break
                    else:
                        return last_unique_c
            else:
                a //= 256


#magic_number = int(open("day21.txt", "r").readlines()[8].split()[1])
magic_number = int(open("input.txt", "r").readlines()[8].split()[1])
print("magic_number = ", magic_number)

print("part 1 = ", run_activation_system(magic_number, True))
print("part 2 = ", run_activation_system(magic_number, False))

