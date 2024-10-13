#include <array>

enum Type {
    INT = 0,
    REF = 1
};

class Layout {
    void* mem;

    public:
    size_t length() {
        return (*(int*) mem);
    }

    Type field_type(size_t ind) {
        Type* ptr = (Type*) (mem + sizeof(int) + sizeof(Type) * ind);
        return *ptr;
    }

    size_t field_offset(size_t ind) {
        size_t result = 0;
        for (size_t i = 0; i < ind; ++i) {
            Type tpe = this->field_type(i);
            if (tpe == Type::INT) {
                result += sizeof(int64_t);
            } else if (tpe == Type::REF) {
                result += sizeof(void*);
            }
        }
        return result;
    }

    size_t mem_size() {
        size_t result = 0;
        for (size_t i = 0; i < this->length(); ++i) {
            Type tpe = this->field_type(i);
            if (tpe == Type::INT) {
                result += sizeof(int64_t);
            } else if (tpe == Type::REF) {
                result += sizeof(void*);
            }
        }
        return result;
    }
};

class Allocation {
    void* mem;

    public:

    explicit Allocation(Layout *layout) {
        int allocation_size = sizeof(Layout*) + sizeof(int) + layout->mem_size();
        this->mem = malloc(allocation_size);
        this->set_layout(layout);
    }


    operator void*() {
        return mem;
    }

    Allocation(void *ptr) {
        this->mem = ptr;
    }

    Layout* layout() {
        return *((Layout**) mem);
    }

    void set_layout(Layout *layout) {
        *((Layout**) mem) = layout;
    }

    int counter() {
        return *((int*) (mem + sizeof(Layout*)));
    }

    void* data() {
        return (mem + sizeof(Layout*) + sizeof(int));
    }

    Allocation dup() {
        return *this;
    }

    void dedup() {
        Layout* layout = this->layout();
        for (size_t i = 0; i < layout->length(); ++i) {
            if (layout->field_type(i) == Type::REF) {
                Allocation* field_ptr = (Allocation*) (this->data() + layout->field_offset(i));
                field_ptr->dedup();
            }
        }
    }
};



