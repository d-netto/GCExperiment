#include <iostream>
#include <list>
#include <string>
#include <vector>

#include <runtime.h>

// ============================================================================
// Garbage collector
// ============================================================================

int64_t mem;
int64_t max_mem = 128 * (1ULL << 20); // 128MB

std::vector<heapval_t *> worklist;
std::list<heapval_t *> heap;

heapval_t *alloc_heapval(void *vtable, size_t size)
{
    auto val = reinterpret_cast<heapval_t *>(
        managed_alloc(sizeof(heapval_t) + size * sizeof(int64_t)));
    val->vtable = vtable;
    val->size = size;
    heap.emplace_back(val);
    return val;
}

heapval_t *alloc_arr(size_t size)
{
    heapval_t *val = reinterpret_cast<heapval_t *>(managed_alloc(sizeof(heapval_t)));
    auto buf = reinterpret_cast<int64_t *>(managed_alloc(size * sizeof(int64_t)));
    val->vtable = buf;
    val->tag = VAL_ARRAY_TAG;
    val->size = size;
    heap.emplace_back(val);
    return val;
}

frame_t *frame_create(void)
{
    auto p = std::malloc(sizeof(frame_t));
    if (p == NULL) {
        std::cerr << "Out of memory" << std::endl;
        exit(1);
    }
    auto frame = reinterpret_cast<frame_t *>(p);
    vector_init(&frame->val_stack, 16);
    vector_init(&frame->locals, 16);
    frame->ip = NULL;
    return frame;
}

void frame_destroy(frame_t *frame)
{
    vector_destroy(&frame->val_stack);
    vector_destroy(&frame->locals);
    free(frame);
}

namespace gc {

// #define GC_ENABBLE_LOG

void gc_log(std::string msg)
{
#ifdef GC_ENABBLE_LOG
    std::cerr << msg << std::endl;
#endif
}

void queue_roots(void)
{
    gc_log("queue_roots");
    frame_t *frame = nullptr;
    for (size_t i = 0; i < frames.size; i++) {
        frame = reinterpret_cast<frame_t *>(vector_get(&frames, i));
        for (size_t j = 0; j < frame->val_stack.size; j++) {
            auto val = reinterpret_cast<heapval_t *>(vector_get(&frame->val_stack, j));
            worklist.push_back(val);
        }
        for (size_t j = 0; j < frame->locals.size; j++) {
            auto val = reinterpret_cast<heapval_t *>(vector_get(&frame->locals, j));
            worklist.push_back(val);
        }
    }
}

void mark(void)
{
    gc_log("mark");
    while (true) {
        if (worklist.empty()) {
            break;
        }
        auto val = worklist.back();
        worklist.pop_back();
        if (val == nullptr) {
            continue;
        }
        int64_t ival = reinterpret_cast<int64_t>(val);
        if (ival & VAL_INT_TAG) {
            continue;
        }
        val->tag |= MARKED_TAG;
        if (val->tag & VAL_ARRAY_TAG) {
            continue;
        }
        auto fstart = reinterpret_cast<int64_t *>(reinterpret_cast<uint8_t *>(val) +
                                                  sizeof(heapval_t));
        auto fend = fstart + val->size;
        for (int64_t *p = fstart; p < fend; p++) {
            auto val = reinterpret_cast<heapval_t *>(*p);
            worklist.push_back(val);
        }
    }
}

void sweep(void)
{
    std::string msg = "sweep start " + std::to_string(mem);
    gc_log(msg);
    for (auto it = heap.begin(); it != heap.end();) {
        auto val = *it;
        if (val->tag & MARKED_TAG) {
            val->tag &= ~MARKED_TAG;
            it++;
        }
        else {
            it = heap.erase(it);
            if (val->tag == VAL_ARRAY_TAG) {
                val->tag &= ~VAL_ARRAY_TAG;
                std::free(val->vtable);
            }
            mem -= sizeof(heapval_t) + val->size * sizeof(int64_t);
            std::free(val);
        }
    }
    msg = "sweep end " + std::to_string(mem);
    gc_log(msg);
}

void gc(void)
{
    queue_roots();
    mark();
    sweep();
}

}

void *managed_alloc(size_t size)
{
    if (mem + size > max_mem) {
        gc::gc();
        if (mem + size > max_mem) {
            std::cerr << "Out of memory" << std::endl;
            exit(1);
        }
    }
    auto ptr = std::calloc(1, size);
    if (ptr == NULL) {
        std::cerr << "Out of memory" << std::endl;
        exit(1);
    }
    mem += size;
    return ptr;
}