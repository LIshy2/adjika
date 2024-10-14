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
        Type* ptr = (Type*) (((char*)mem) + sizeof(int) + sizeof(Type) * ind);
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
        size_t allocation_size = sizeof(Layout*) + sizeof(int64_t) + layout->mem_size();
        this->mem = ((char*) malloc(allocation_size)) + sizeof(Layout*) + sizeof(int64_t);
        this->set_layout(layout);
    }


    operator void*() {
        return mem;
    }

    Allocation(void *ptr) {
        this->mem = ptr;
    }

    Layout* layout() {
        return *((Layout**) (((char*) mem) - sizeof(int64_t) - sizeof(Layout*)));
    }

    void set_layout(Layout *layout) {
        *((Layout**) (((char*) mem) - sizeof(int64_t) - sizeof(Layout*))) = layout;
    }

    int counter() {
        return *((int64_t*) (((char*) mem) - sizeof(int64_t*)));
    }

    void* data() {
        return (mem);
    }

    Allocation dup() {
        int64_t* counter = (int64_t*) (((char*) mem) - sizeof(int64_t*));
        *counter += 1;
        return *this;
    }

    void dedup() {
        int64_t* counter = (int64_t*) (((char*) mem) - sizeof(int64_t*));
        *counter -= 1;
        if (*counter == 0) {
            Layout* layout = this->layout();
            for (size_t i = 0; i < layout->length(); ++i) {
                if (layout->field_type(i) == Type::REF) {
                    Allocation* field_ptr = (Allocation*) (((char*) this->data()) + layout->field_offset(i));
                    field_ptr->dedup();
                }
            }
            free(((char*) mem) - sizeof(int64_t) - sizeof(Layout*));
        }
    }
};



