#include "aoc.h"

#define SIZE 101
#define WORKERS 5
#define DURATION(c) (61 + (int)((c) - 'A'))

typedef char Task;

struct Worker {
    int end;
    Task task;

    Worker() : end(0), task(0) {}
    ~Worker() {}
};

// Finds the next available task, where one has not been started, but all dependencies have been completed
// Adds this task to `started`, and returns it.
Task next_task(
        std::map<Task, std::string>& dependencies,
        std::unordered_set<Task>& complete,
        std::unordered_set<Task>& started);


main {
    std::map<Task, std::string> dependencies; // post -> pre
    for (int i = 0; i < SIZE; i++) {
        Task pre, post;
        assert(scanf("Step %c must be finished before step %c can begin.\n", &pre, &post) == 2, "reading input");
        
        dependencies[post].push_back(pre);
        dependencies[pre]; // Puts `pre` into the dependencies, with no value (empty string), so it gets sorted
    }

    // Part 1:
    // Complete the tasks in dependency order, then sort order
    std::unordered_set<Task> complete;
    std::string order;
    for (int i = 0; i < SIZE; i++) {
        // Iterate once per each task, until complete
        // Tasks are completed instantly, so 'complete' and 'started' are the same
        Task task = next_task(dependencies, complete, complete);
        order.push_back(task);
    }

    println("Part 1: %s", order.c_str());

    complete.clear();

    // Part 2: Flashbacks to that Gantt chart I had to do for that one engineering course...
    // It was terrible, but ya know even doing it with C++, I can't tell if it's more or less terrible.
    std::unordered_set<Task> started;
    Worker* workers = new Worker[WORKERS];
    for (int t = 0;; t++) {
        // Finish existing started tasks
        for (int i = 0; i < WORKERS; i++) {
            Worker* worker = &workers[i];
            if (worker->task != 0 && worker->end == t) { // Worker is finished
                complete.insert(worker->task); // Task is complete, so add it to completed list
                worker->task = 0;
            }
        }

        // Check if all tasks are complete
        if (complete.size() == dependencies.size()) {
            println("Part 2: %d", t);
            break;
        }

        // Start new tasks
        for (int i = 0; i < WORKERS; i++) {
            Worker* worker = &workers[i];
            if (worker->task == 0) { // Worker is not working on something, so find them another task!
                Task task = next_task(dependencies, complete, started); // Task gets started, but not completed
                if (task != 0) { // If there is an available task to complete
                    worker->task = task;
                    worker->end = t + DURATION(task);
                }
            }
        }
    }
}

Task next_task(
        std::map<Task, std::string>& dependencies,
        std::unordered_set<Task>& complete,
        std::unordered_set<Task>& started) {
    // Iterate all the incomplete tasks, then check each of their dependencies
    // Pick the first task that has complete dependencies
    for (auto dep = dependencies.begin(); dep != dependencies.end(); dep++) {
        if (started.find(dep->first) == started.end()) { // Task is not started, meaning we can start it
            bool ready = true;
            for (auto pre = dep->second.begin(); pre != dep->second.end(); pre++) {
                if (complete.find(*pre) == complete.end()) { // Prerequisite task is not complete
                    ready = false;
                    break;
                }
            }
            if (ready) {
                started.insert(dep->first);
                return dep->first;
            }
        }
    }
    return 0;
}