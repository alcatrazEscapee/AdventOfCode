#ifndef POINT_H
#define POINT_H

#include "../src/aoc.h"

/// @brief A basic 2D point, which supports +, - ==, != operators.
class Point {
public:

    Point() = default;
    Point(int x, int y) : x(x), y(y) {}

    Point& operator+=(const Point& rhs) { x += rhs.x; y += rhs.y; return *this; }
    Point& operator-=(const Point& rhs) { x -= rhs.x; y -= rhs.y; return *this; }

    const Point operator+(const Point& other) const { return Point(*this) += other; }
    const Point operator-(const Point& other) const { return Point(*this) -= other; }

    bool operator==(const Point& other) const = default;

    int x, y;

    struct Hash {
        size_t operator()(const Point& point) const {
            return (std::hash<int>()(point.x) << 1) ^ (std::hash<int>()(point.y));
        }
    };
};

#endif