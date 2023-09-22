#include "aoc.h"

#define SIZE 966

itype(unsigned char, 1) u8;

enum class Action {
    Start, Sleep, Wake
};

struct Guard {
    u8 asleep[60];
    int total;
    int id;
    int max_minute;

public:
    int answer() const {
        return id * max_minute;
    }

    int max_asleep() const {
        return (int) asleep[max_minute];
    }

    void calculate_max_minute_asleep() {
        const u8* start = asleep;
        const u8* end = std::next(asleep, 60);
        max_minute = std::distance(start, std::max_element(start, end));
    }
};

struct Record {
    int year, month, day, hr, min;
    int id;
    Action action;
    Guard* guard;

    int timestamp() const {
        return min + 60 * (hr + 24 * day);
    }

    bool operator==(const Record& other) const = default;
    auto operator<=>(const Record& other) const = default;
};



main {
    // Parse input
    Record* records = new Record[SIZE];
    for (int i = 0; i < SIZE; i++) {
        Record& record = records[i];
        
        std::string line;
        std::getline(std::cin, line);

        sscanf(line.c_str(), "[%d-%d-%d %d:%d] ", &record.year, &record.month, &record.day, &record.hr, &record.min);

        if (line.find("wakes up") != std::string::npos) {
            record.action = Action::Wake;
        } else if (line.find("falls asleep") != std::string::npos) {
            record.action = Action::Sleep;
        } else {
            sscanf(line.substr(19).c_str(), "Guard #%d", &record.id);
        }
    }

    // Sort the list of records chronologically
    // Then we can fill in the missing guard IDs by iterating forward
    std::sort(records, records + SIZE);

    // Iterate through each action, and store the guards' minutes spent asleep
    std::unordered_map<int, Guard> guards;
    int id = 0, stamp = 0;
    for (int i = 0; i < SIZE; i++) {
        Record* record = &records[i];
        switch (record->action) {
            case Action::Start:
                id = record->id;
                break;
            case Action::Sleep:
                stamp = record->timestamp();
                break;
            case Action::Wake:
                Guard& guard = guards[id];
                guard.id = id;
                guard.total += record->timestamp() - stamp;
                for (int m = stamp; m < record->timestamp(); m++) {
                    guard.asleep[m % 60]++;
                }
                break;
        }
    }

    // Calculate the max minute spent asleep, per guard
    for (auto& guard : guards) {
        guard.second.calculate_max_minute_asleep();
    }

    // Part 1: the guard who spent the most total minutes asleep
    const Guard& max_guard = std::max_element(
        guards.begin(), guards.end(),
        [](const std::pair<int, Guard>& a, const std::pair<int, Guard>& b) -> bool { 
            return a.second.total < b.second.total;
        })->second;
    
    println("Part 1: %d", max_guard.answer());

    // Part 2: the minute that was the most spent asleep across all guards and minutes
    const Guard& max_guard_minute = std::max_element(
        guards.begin(), guards.end(),
        [](const std::pair<int, Guard>& a, const std::pair<int, Guard>& b) -> bool {
            return a.second.max_asleep() < b.second.max_asleep();
        })->second;
    
    println("Part 2: %d", max_guard_minute.answer());
}