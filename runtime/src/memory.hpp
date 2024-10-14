#include <array>

enum Type {
    INT = 0,
    REF = 1
};

class Layout {
    void* mem;

    public:
    size_t length();

    Type field_type(size_t ind);

    size_t field_offset(size_t ind);

    size_t mem_size();
};

class Allocation {
    void* mem;

    public:

    explicit Allocation(Layout *layout);

    operator void*();

    Allocation(void *ptr);

    Layout* layout();

    void set_layout(Layout *layout);

    int counter();

    void* data();

    Allocation dup();

    void dedup();
};



