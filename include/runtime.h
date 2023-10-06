#ifndef _RUNTIME_H_
#define _RUNTIME_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <assert.h>
#include <stdint.h>

void *managed_alloc(size_t size);

typedef struct {
    union {
        uint8_t tag : 3;
        void *vtable;
    };
    int64_t size;
    // payload
} heapval_t;

#define VAL_INT_TAG (1 << 0)

#define MARKED_TAG (1 << 0)
#define VAL_ARRAY_TAG (1 << 1)
#define VAL_FRAME_TAG (1 << 2)

static inline int64_t *pith_field(heapval_t *val, size_t idx)
{
    return (int64_t *)((uint8_t *)val + sizeof(heapval_t) + idx * sizeof(int64_t));
}

static inline int64_t *pith_field_arr(heapval_t *val, size_t idx)
{
    return (int64_t *)((uint8_t *)val->vtable + idx * sizeof(int64_t));
}

static inline int64_t ptr_to_int(void *ptr)
{
    int64_t iptr = (int64_t)ptr;
    assert(iptr & VAL_INT_TAG);
    return (iptr >> 1);
}

static inline void *int_to_ptr(int64_t i)
{
    return (void *)((i << 1) | VAL_INT_TAG);
}

static inline heapval_t *ptr_to_hval(void *ptr)
{
    assert(!((int64_t)ptr & VAL_INT_TAG));
    return (heapval_t *)((int64_t)ptr);
}

heapval_t *alloc_heapval(void *vtable, size_t size);
heapval_t *alloc_arr(size_t size);

typedef struct {
    void **buffer;
    size_t size;
    size_t capacity;
} vector_t;

static inline void vector_init(vector_t *vec, size_t capacity)
{
    vec->buffer = (void **)malloc(capacity * sizeof(void *));
    if (vec->buffer == NULL) {
        fprintf(stderr, "vector_init: malloc failed\n");
        exit(1);
    }
    vec->size = 0;
    vec->capacity = capacity;
}

static inline void vector_push(vector_t *vec, void *elem)
{
    if (vec->size == vec->capacity) {
        vec->capacity *= 2;
        vec->buffer = (void **)realloc(vec->buffer, vec->capacity * sizeof(void *));
        if (vec->buffer == NULL) {
            fprintf(stderr, "vector_push: realloc failed\n");
            exit(1);
        }
    }
    vec->buffer[vec->size++] = elem;
}

static inline void *vector_pop(vector_t *vec)
{
    if (vec->size == 0) {
        fprintf(stderr, "vector_pop: empty vector\n");
        exit(1);
    }
    return vec->buffer[--vec->size];
}

static inline void vector_set(vector_t *vec, size_t idx, void *elem)
{
    if (idx >= vec->size) {
        fprintf(stderr, "vector_set: index out of bounds\n");
        exit(1);
    }
    vec->buffer[idx] = elem;
}

static inline void *vector_get(vector_t *vec, size_t idx)
{
    if (idx >= vec->size) {
        fprintf(stderr, "vector_get: index out of bounds\n");
        exit(1);
    }
    return vec->buffer[idx];
}

static inline void *vector_top(vector_t *vec)
{
    if (vec->size == 0) {
        fprintf(stderr, "vector_top: empty vector\n");
        exit(1);
    }
    return vec->buffer[vec->size - 1];
}

static inline void vector_destroy(vector_t *vec)
{
    free(vec->buffer);
    vec->buffer = NULL;
    vec->size = 0;
    vec->capacity = 0;
}

typedef struct {
    vector_t val_stack;
    vector_t locals;
    void *ip_start;
    void *ip;
} frame_t;

frame_t *frame_create(void);
void frame_destroy(frame_t *frame);

extern vector_t frames;
extern frame_t *fp;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _RUNTIME_H_