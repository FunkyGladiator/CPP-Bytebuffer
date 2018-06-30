#pragma once
#include <new>
#include <vector>
#include <cassert>
#include <ostream>
#include <type_traits>
#include <algorithm>

//Define macro for getting function signature with most popular compilers (used for making static_asserts more verbose)
#if defined(_MSC_VER)
#   define BYTEBUFFER_FUNCSIG __FUNCSIG__ " - "
#elif defined(__GNUC__) || defined(__clang__)
#   define BYTEBUFFER_FUNCSIG __PRETTY_FUNCTION__ " - "
#else
#   define BYTEBUFFER_FUNCSIG ""
#endif

typedef uint8_t byte;
typedef std::vector<byte>::allocator_type bytebuffer_allocator;
typedef std::vector<byte>::iterator bytebuffer_iterator;
typedef std::vector<byte>::const_iterator const_bytebuffer_iterator;
typedef std::vector<byte>::reverse_iterator reverse_bytebuffer_iterator;
typedef std::vector<byte>::const_reverse_iterator const_reverse_bytebuffer_iterator;

//STL compatible implementation of a bytebuffer
class ByteBuffer
{
private:
    size_t offset = 0;
    size_t marked_offset = 0;

    //ByteBuffer is backed by a vector, vector ensures that its components are aligned in memory and allows us to delegate some common functionality to it
    std::vector<byte> buffer;

    //Default ByteBuffer capacity
    static constexpr size_t default_capacity = 128;
    //Default growing behavior
    static constexpr bool default_auto_grow = true;
    //Whether a ByteBuffer will grow or assert, if a write exceeds its capacity 
    const bool auto_grow;

public:
    //Construct new ByteBuffer with specified capacity expressed in bytes
    template<typename T, typename = std::enable_if<std::is_arithmetic<T>::value>::type> //enable_if necessary since bool can be implicitly converted to any arithmetic_type
    explicit ByteBuffer(T capacity, bool auto_grow = default_auto_grow, bytebuffer_allocator allocator = std::allocator<byte>()) :
        auto_grow(auto_grow), buffer(allocator)
    {
        buffer.reserve(capacity);
    }
    //Construct new ByteBuffer with the specified growing behavior
    explicit ByteBuffer(bool auto_grow, bytebuffer_allocator allocator = std::allocator<byte>()) :
        ByteBuffer(default_capacity, auto_grow, allocator)
    {}
    //Construct new ByteBuffer
    explicit ByteBuffer(bytebuffer_allocator allocator = std::allocator<byte>()) :
        ByteBuffer(default_capacity, default_auto_grow, allocator)
    {}

    //Construct new ByteBuffer using iterators
    template<typename Iterator, typename = std::enable_if<!std::is_same<typename std::iterator_traits<Iterator>::value_type, void>::value>>
    explicit ByteBuffer(Iterator begin, Iterator end, bool auto_grow, bytebuffer_allocator allocator = std::allocator<byte>()) :
        auto_grow(auto_grow), buffer(allocator)
    {
        typedef std::iterator_traits<Iterator>::value_type T;
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::ByteBuffer(Iterator, Iterator, bool), Iterator must iterate over a trivially_copyable type");
        buffer.resize(mem_size(begin, end));
        iterative_append(begin, end);
    }
    //Construct new ByteBuffer using iterators
    template<typename Iterator, typename = std::enable_if<!std::is_same<typename std::iterator_traits<Iterator>::value_type, void>::value>>
    explicit ByteBuffer(Iterator begin, Iterator end, bytebuffer_allocator allocator = std::allocator<byte>()) :
        ByteBuffer(begin, end, default_auto_grow, allocator)
    {}

    //Construct new ByteBuffer with initializer list
    template<typename T>
    explicit ByteBuffer(std::initializer_list<T> list, bool auto_grow, bytebuffer_allocator allocator = std::allocator<byte>()) :
        auto_grow(auto_grow), buffer(allocator)
    {
        //We must ensure that T is_trivially_copyable because resizing buffer results in a direct memory copy, in addition to that I check for is_copy_constructible to disable function if copy constructor has explicitly been deleted
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::ByteBuffer(std::initializer_list<T>, bool), T must be trivially_copyable");
        buffer.resize(list.size() * sizeof(T));
        iterative_append(list.begin(), list.end());
    }
    //Construct new ByteBuffer with initializer list
    template<typename T>
    explicit ByteBuffer(std::initializer_list<T> list, bytebuffer_allocator allocator = std::allocator<byte>()) :
        ByteBuffer(list, default_auto_grow, allocator)
    {}

    //Reset offset
    void flip() { offset = 0; }
    //Clear bytebuffer, then reset offset and marked_offset (leaves capacity unchanged)
    void clear() { buffer.clear(); offset = 0; marked_offset = 0; }
    //ByteBuffer's size in bytes
    size_t size() const { return buffer.size(); }
    //ByteBuffer's capacity in bytes
    size_t capacity() const { return buffer.capacity(); }
    //Gets the number of remaining readable bytes
    size_t remaining() const { return buffer.size() - offset; }
    //Marks an offset on this ByteBuffer to be used later
    void mark(size_t offset) { assert(offset < buffer.size()); marked_offset = offset; }
    //Shrink bytebuffer's capacity to fit its size
    void shrink_to_fit() { buffer.shrink_to_fit(); }
    //Indicates whether the bytebuffer is empty or not
    bool empty() const { return buffer.empty(); }
    //Ensure that the ByteBuffer's capacity be at least enough to contain n elements (provided that n < max_size).
    void ensure_capacity(size_t n) { buffer.reserve(n); }
    //Return maximum possible length of sequence
    size_t max_size() const { return buffer.max_size(); }
    //Resets this ByteBuffer's offset. If an offset has been marked through 'mark()' before, 'offset' will be set to 'marked_offset', which will then be discarded.
    //If no offset has been marked, sets 'offset' = 0
    void reset() { offset = marked_offset; marked_offset = 0; }
    //Provide specialization for std::swap (can be found using argument-dependent lookup, defined for C++11 and up). Swaps bytes, offset and marked_offset
    friend void swap(ByteBuffer& a, ByteBuffer& b) { std::swap(a.buffer, b.buffer); std::swap(a.offset, b.offset); std::swap(a.marked_offset, b.marked_offset); }
    //Get allocator
    bytebuffer_allocator get_allocator() { return buffer.get_allocator(); }

    //Relative read without moving offset
    template<typename T>
    const T& peek()
    {
        return read<T>(offset);
    }

    //Relative read
    template<typename T>
    const T& read()
    {
        const T& data = read<T>(offset);
        offset += sizeof(T);
        return data;
    }

    //Absolute read
    template<typename T> const
    const T& read(size_t position)
    {
        assert(position + sizeof(T) <= buffer.size());
        return *(reinterpret_cast<T*>(&buffer[position]));
    }

    //Constructs a new element of type T at offset using args as the arguments for its construction.
    template<typename T, typename... Args>
    T& emplace(Args... args)
    {
        //We must ensure that T is_trivially_copyable because resizing buffer results in a direct memory copy, in addition to that I check for is_copy_constructible to disable function if copy constructor has explicitly been deleted
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::emplace<T, ...Args>(Args... args), T must be trivially_copyable");
        offset += sizeof(T);
        ensure_size(offset);
        return *new(&buffer[offset - sizeof(T)]) T(args...);
    }

    //Append data of type T to ByteBuffer
    template<typename T>
    T& put(const T& data)
    {
        //memcpy is only safe for trivially_copyable types, in addition to that I check for is_copy_constructible to disable function if copy constructor has explicitly been deleted
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::put<T>(const T &), T must be trivially_copyable");
        return *reinterpret_cast<T*>(memcpy_append(&data, sizeof(T)));
    }
    //Append specified amount of objects of type T to ByteBuffer from array
    template<typename T>
    T& put_array(const T* data, const size_t amount)
    {
        //memcpy is only safe for trivially_copyable types, in addition to that I check for is_copy_constructible to disable function if copy constructor has explicitly been deleted
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::put<T>(const void *, const size_t), T must be trivially_copyable");
        return *reinterpret_cast<T*>(memcpy_append(data, sizeof(T) * amount));
    }
    
    //Append data using iterators
    template<typename Iterator, typename = std::enable_if<!std::is_same<typename std::iterator_traits<Iterator>::value_type, void>::value>>
    typename std::iterator_traits<Iterator>::value_type& put(Iterator begin, Iterator end)
    {
        typedef std::iterator_traits<Iterator>::value_type T;
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::ByteBuffer(Iterator, Iterator, bool), Iterator must iterate over a trivially_copyable type");
        return *reinterpret_cast<T*>(iterative_append(begin, end));
    }
    //Append data from initializer list (delegated to iterator put)
    template<typename T>
    T& put(std::initializer_list<T> data)
    {
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::put<T>(std::initializer_list<T>), T must be trivially_copyable");
        return put(data.begin(), data.end());
    }

    //Push ByteBuffer as ascii to ostream
    friend std::ostream& operator<<(std::ostream& os, const ByteBuffer buffer)
    {
        for (size_t i = 0; i < buffer.size(); ++i)
            os << buffer[i];
        return os;
    }

    //Write data from ByteBuffer to output variable
    template<typename T>
    ByteBuffer& operator>>(T& output)
    {
        output = read<T>();
        return *this;
    }
    //Append data to ByteBuffer
    template<typename T>
    ByteBuffer& operator<<(const T& data)
    {
        //memcpy is only safe for trivially_copyable types, in addition to that I check for is_copy_constructible to disable function if copy constructor has explicitly been deleted
        static_assert(std::is_trivially_copyable<T>::value && std::is_copy_constructible<T>::value,
            BYTEBUFFER_FUNCSIG "ByteBuffer::operator<<(const T &), T must be trivially_copyable");
        memcpy_append(&data, sizeof(T));
        return *this;
    }

    //Common operators and iterators are delegated to std::vector (saving us from alot of work)
    byte& operator[](const size_t pos) { return buffer[pos]; }
    const byte& operator[](const size_t pos) const { return buffer[pos]; }
    bool operator==(const ByteBuffer& other) const { return buffer == other.buffer; }
    bool operator!=(const ByteBuffer& other) const { return buffer != other.buffer; }
    bool operator <(const ByteBuffer& other) const { return buffer <  other.buffer; }
    bool operator >(const ByteBuffer& other) const { return buffer >  other.buffer; }
    bool operator<=(const ByteBuffer& other) const { return buffer <= other.buffer; }
    bool operator>=(const ByteBuffer& other) const { return buffer >= other.buffer; }

    bytebuffer_iterator begin() { return buffer.begin(); }
    bytebuffer_iterator end() { return buffer.end(); }
    const_bytebuffer_iterator begin() const { return buffer.begin(); }
    const_bytebuffer_iterator end() const { return buffer.end(); }

    reverse_bytebuffer_iterator rbegin() { return buffer.rbegin(); }
    reverse_bytebuffer_iterator rend() { return buffer.rend(); }
    const_reverse_bytebuffer_iterator rbegin() const { return buffer.rbegin(); }
    const_reverse_bytebuffer_iterator rend() const { return buffer.rend(); }

    const_bytebuffer_iterator cbegin() const { return buffer.cbegin(); }
    const_bytebuffer_iterator cend() const { return buffer.cend(); }
    const_reverse_bytebuffer_iterator crbegin() const { return buffer.crbegin(); }
    const_reverse_bytebuffer_iterator crend() const { return buffer.crend(); }
    
private:
    //Append data using memcpy
    void* memcpy_append(const void* data, const size_t data_size)
    {
        offset += data_size;
        ensure_size(offset);
        return memcpy(&buffer[offset - data_size], reinterpret_cast<const byte*>(data), data_size);
    }
    //Append data iteratively
    template<typename Iterator>
    void* iterative_append(Iterator begin, Iterator end)
    {
        typedef std::iterator_traits<Iterator>::value_type T;
        auto starting_offset = offset;
        for (; begin != end; ++begin)
        {
            ensure_size(offset + sizeof(T));
            *reinterpret_cast<T*>(&buffer[offset]) = *begin;
            offset += sizeof(T);
        }
        return &buffer[starting_offset];
    }
    //Get the size of the memory block from the begin iterator till the end iterator (complexity O(n))
    template<typename Iterator>
    size_t mem_size(Iterator begin, Iterator end)
    {
        return abs(std::distance(begin, end)) * sizeof(std::iterator_traits<Iterator>::value_type);
    }
    //Ensure that ByteBuffer has a size of atleast the new specified size.
    //If auto_grow is enabled this function can increase the ByteBuffer's capacity, if auto_grow is disabled this function will fail to ensure a size beyond the buffer's capacity 
    void ensure_size(size_t size)
    {
        if (auto_grow)
        {
            if (buffer.capacity() < size)
            {
                //Makes sure that ByteBuffer is backed by a buffer of atleast the specified capacity. If current capacity is exceeded, it will be doubled.
                //If double the current capacity is less than the required capacity, then the required capacity will be used instead.
                if (buffer.capacity() * 2 > size)
                    buffer.reserve(buffer.capacity() * 2);
                else
                    buffer.reserve(size);
                buffer.resize(size);
            }
            else if (buffer.size() <= size)
            {
                buffer.resize(size);
            }
        }
        else if (buffer.size() <= size && buffer.capacity() >= size) //Check for capacity to make sure no reallocations can take place
        {
            buffer.resize(size);
        }
        assert(buffer.size() >= size);
    }
};

//Undefine macro to prevent pollution of the global namespace
#undef BYTEBUFFER_FUNCSIG
