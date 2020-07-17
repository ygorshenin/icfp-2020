#!/usr/bin/env python3

import sys


class Cons(object):
    def __init__(self, first, second):
        self.first = first
        self.second = second

    def __repr__(self):
        return "(cons {} {})".format(self.first, self.second)


def decode(s):
    if s.startswith("11"):
        a, s = decode(s[2:])
        b, s = decode(s)
        return Cons(a, b), s

    if s.startswith("00"):
        return None, s[2:]

    if s.startswith("01") or s.startswith("10"):
        sign = 1
        if s.startswith("10"):
            sign = -1
        i = 2
        width = 0
        while i < len(s) and s[i] == '1':
            width += 1
            i += 1
        assert i < len(s) and s[i] == '0', 'Failed to decode: ' + s
        width *= 4
        i += 1

        assert i + width <= len(s), 'Failed to decode: ' + s
        if width == 0:
            return 0, s[i + width:]
        return int(s[i:i + width], 2), s[i + width:]

    assert False, 'Failed to decode: ' + s


if __name__ == '__main__':
    for line in sys.stdin.readlines():
        line = line.strip()
        result, s = decode(line)
        assert len(s) == 0, 'Failed to parse full line: {}, rest: {}'.format(line, s)
        print(result)
