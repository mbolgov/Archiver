#include <fstream>
#include <functional>
#include <iostream>
#include <queue>
#include <unordered_map>
#include <cstdint>

const uint8_t COUNT_BITS = 9;
const uint16_t MAX_SYMBOLS_COUNT = 259;
const uint16_t FILENAME_END = 256;
const uint16_t ONE_MORE_FILE = 257;
const uint16_t ARCHIVE_END = 258;

struct Node {
    uint16_t ch;
    size_t priority;
    Node *left = nullptr, *right = nullptr;

    bool operator<(const Node& other) {
        return std::tie(priority, ch) < std::tie(other.priority, other.ch);
    }

    ~Node() {
        if (left) {
            delete left;
        }
        if (right) {
            delete right;
        }
    }
};

class NodeComp {
public:
    bool operator()(Node* lhs, Node*& rhs) {
        return std::tie(lhs->priority, lhs->ch) < std::tie(rhs->priority, rhs->ch);
    }
};

template <typename Type, class Compare>
class Heap {
public:
    Heap() {
        size_ = 0;
        list_ = {};
    }

    size_t size() const {  // NOLINT
        return size_;
    }
    Type top() const {  // NOLINT
        return list_[0];
    }
    void pop() {  // NOLINT
        std::swap(list_[0], list_[--size_]);
        list_.pop_back();
        SiftDown(0);
    }
    void push(const Type& new_element) {  // NOLINT
        list_.push_back(new_element);
        SiftUp(size_++);
    }

protected:
    size_t size_;
    std::vector<Type> list_;

    void SiftDown(size_t i) {
        while (2 * i + 1 < size_) {
            size_t left = 2 * i + 1;
            size_t right = 2 * i + 2;
            size_t j = left;
            if (right < size_ && Compare()(list_[right], list_[left])) {
                j = right;
            }
            if (Compare()(list_[i], list_[j])) {
                break;
            }
            std::swap(list_[i], list_[j]);
            i = j;
        }
    }
    void SiftUp(size_t i) {
        while (i != 0 && Compare()(list_[i], list_[(i - 1) / 2])) {
            std::swap(list_[i], list_[(i - 1) / 2]);
            i = (i - 1) / 2;
        }
    }
};

void AddSymbol(Node* root, const uint16_t& ch, size_t i, const std::string& code) {
    if (i == code.size()) {
        root->ch = ch;
    } else if (code[i] == '0') {
        if (!root->left) {
            root->left = new Node();
        }
        AddSymbol(root->left, ch, i + 1, code);
    } else {
        if (!root->right) {
            root->right = new Node();
        }
        AddSymbol(root->right, ch, i + 1, code);
    }
}

uint16_t Decode(Node* root, size_t& index, std::ifstream& fin) {
    if (!root->left && !root->right) {
        return root->ch;
    }
    char ch = 0;
    fin.get(ch);
    ++index;
    if (ch == '0') {
        return Decode(root->left, index, fin);
    }
    return Decode(root->right, index, fin);
}

Node* HuffmanTree(size_t frequency[]) {
    Heap<Node*, NodeComp> queue;
    for (uint16_t sym = 0; sym < MAX_SYMBOLS_COUNT; ++sym) {
        if (frequency[sym] != 0) {
            queue.push(new Node{sym, frequency[sym]});
        }
    }

    while (queue.size() != 1) {
        Node* left = queue.top();
        queue.pop();
        Node* right = queue.top();
        queue.pop();
        queue.push(new Node{std::min(left->ch, right->ch), left->priority + right->priority, left, right});
    }

    return queue.top();
}

void EncodeNumber(const uint16_t& num, std::ofstream& fout) {
    for (int16_t i = COUNT_BITS - 1; i >= 0; --i) {
        fout << (num & (1 << i) ? 1 : 0);
    }
}

uint16_t DecodeNumber(size_t& index, std::ifstream& fin) {
    uint16_t result = 0;
    for (uint16_t i = 0; i < COUNT_BITS; ++i, ++index) {
        char ch = 0;
        fin.get(ch);
        if (ch == '1') {
            result |= (1 << i);
        }
    }
    return result;
}

void EncodeHuffmanTree(Node* root, uint16_t& symbols_count, std::string huffman_code[], std::ofstream& fout) {
    std::queue<std::pair<Node*, uint16_t>> queue;
    queue.push({root, 0});

    uint16_t lengths[MAX_SYMBOLS_COUNT];
    std::fill(lengths, lengths + MAX_SYMBOLS_COUNT, 0);
    while (!queue.empty()) {
        auto [current_root, code_len] = queue.front();
        queue.pop();
        if (!current_root->left && !current_root->right) {
            ++symbols_count;
            lengths[current_root->ch] = code_len;
        } else {
            queue.push({current_root->left, code_len + 1});
            queue.push({current_root->right, code_len + 1});
        }
    }

    std::vector<std::vector<uint16_t>> symbols_with_code_length(MAX_SYMBOLS_COUNT);
    for (uint16_t ch = 0; ch < MAX_SYMBOLS_COUNT; ++ch) {
        if (lengths[ch] != 0) {
            symbols_with_code_length[lengths[ch] - 1].push_back(ch);
        }
    }

    EncodeNumber(symbols_count, fout);

    uint16_t last_len = 0;
    std::string last_code;
    for (uint16_t length = 1; length <= MAX_SYMBOLS_COUNT; ++length) {
        for (auto& ch : symbols_with_code_length[length - 1]) {
            EncodeNumber(ch, fout);
            if (last_code.empty()) {
                huffman_code[ch] = std::string(length, '0');
            } else {
                huffman_code[ch] = last_code;
                size_t j = last_len - 1;
                while (huffman_code[ch][j] == '1') {
                    huffman_code[ch][j] = '0';
                    --j;
                }
                huffman_code[ch][j] = '1';
                huffman_code[ch].append(length - last_len, '0');
            }
            last_len = length;
            last_code = huffman_code[ch];
        }
    }

    uint16_t current_lengths_count = 0;
    for (uint16_t length = 1; length <= MAX_SYMBOLS_COUNT && current_lengths_count < symbols_count; ++length) {
        current_lengths_count += symbols_with_code_length[length - 1].size();
        EncodeNumber(symbols_with_code_length[length - 1].size(), fout);
    }
}

int main(int argc, char** argv) {
    if (static_cast<size_t>(argc) == 1) {
        std::cout << "Not enough arguments" << std::endl;
        return 111;
    }

    std::string command = static_cast<std::string>(argv[1]);

    if (command == "-c") {
        if (argc < 4) {
            std::cout << "Nothing to archive" << std::endl;
            return 111;
        }

        size_t frequency[MAX_SYMBOLS_COUNT];
        std::fill(frequency, frequency + MAX_SYMBOLS_COUNT, 0);

        std::ifstream fin;
        for (size_t i = 3; i < static_cast<size_t>(argc); ++i) {
            for (char& ch : static_cast<std::string>(argv[i])) {
                ++frequency[static_cast<uint16_t>(ch)];
            }
            ++frequency[FILENAME_END];
            fin.open(argv[i], std::ios::binary);
            char ch = 0;
            while (fin.get(ch)) {
                ++frequency[static_cast<uint16_t>(static_cast<unsigned char>(ch))];
            }
            fin.close();
            ++frequency[ONE_MORE_FILE];
        }
        --frequency[ONE_MORE_FILE];
        ++frequency[ARCHIVE_END];

        Node* root = HuffmanTree(frequency);
        std::string huffman_code[MAX_SYMBOLS_COUNT];

        std::ofstream fout;
        fout.open(argv[2], std::ios::binary);
        uint16_t symbols_count = 0;
        EncodeHuffmanTree(root, symbols_count, huffman_code, fout);
        delete root;

        for (size_t i = 3; i < static_cast<size_t>(argc); ++i) {
            for (char& ch : static_cast<std::string>(argv[i])) {
                fout << huffman_code[static_cast<uint16_t>(ch)];
            }
            fout << huffman_code[FILENAME_END];
            fin.open(argv[i], std::ios::binary);
            char ch = 0;
            while (fin.get(ch)) {
                fout << huffman_code[static_cast<uint16_t>(static_cast<unsigned char>(ch))];
            }
            fin.close();
            if (i + 1 != static_cast<size_t>(argc)) {
                fout << huffman_code[ONE_MORE_FILE];
            }
        }
        fout << huffman_code[ARCHIVE_END];
        fout.close();
        return 0;
    }

    if (command == "-d") {
        if (argc == 2) {
            std::cout << "nothing to unzip" << std::endl;
            return 111;
        }
        if (argc > 3) {
            std::cout << "too many files to unzip" << std::endl;
            return 111;
        }

        std::ifstream fin;
        fin.open(argv[2], std::ios::binary);
        size_t index = 0;

        uint16_t symbols_count = DecodeNumber(index, fin);
        uint16_t symbols[symbols_count];
        for (uint16_t i = 0; i < symbols_count; ++i) {
            symbols[i] = DecodeNumber(index, fin);
        }

        uint16_t lengths[MAX_SYMBOLS_COUNT];
        uint16_t current_lengths_count = 0;
        for (uint16_t length = 1, i = 0; length <= MAX_SYMBOLS_COUNT && current_lengths_count < symbols_count;
             ++length) {
            uint16_t length_count = DecodeNumber(index, fin);
            for (uint16_t current_length_count = 0; current_length_count < length_count; ++current_length_count, ++i) {
                lengths[symbols[i]] = length;
            }
            current_lengths_count += length_count;
        }

        std::string huffman_code[MAX_SYMBOLS_COUNT];
        huffman_code[symbols[0]] = std::string(lengths[symbols[0]], '0');
        for (uint16_t i = 1; i < symbols_count; ++i) {
            huffman_code[symbols[i]] = huffman_code[symbols[i - 1]];
            size_t j = lengths[symbols[i - 1]] - 1;
            while (huffman_code[symbols[i]][j] == '1') {
                huffman_code[symbols[i]][j] = '0';
                --j;
            }
            huffman_code[symbols[i]][j] = '1';
            huffman_code[symbols[i]].append(lengths[symbols[i]] - lengths[symbols[i - 1]], '0');
        }

        Node* root = new Node();
        for (uint16_t i = 0; i < symbols_count; ++i) {
            AddSymbol(root, symbols[i], 0, huffman_code[symbols[i]]);
        }

        uint16_t ch = 0;
        while (ch != ARCHIVE_END) {
            std::string file_name;
            ch = Decode(root, index, fin);
            while (ch != FILENAME_END) {
                file_name.push_back(static_cast<char>(ch));
                ch = Decode(root, index, fin);
            }
            std::ofstream fout;
            fout.open(file_name, std::ios::binary);
            ch = Decode(root, index, fin);
            while (ch != ONE_MORE_FILE && ch != ARCHIVE_END) {
                fout << static_cast<char>(ch);
                ch = Decode(root, index, fin);
            }
            fout.close();
        }

        delete root;
        fin.close();

        return 0;
    }

    if (command == "-h") {
        if (argc != 2) {
            std::cout << "too many arguments for command -h" << std::endl;
            return 111;
        }
        std::cout << "archiver -c archive_name file1[file2...] - archive files file1, file2, ... and save the result "
                     "to archive_name."
                  << std::endl;
        std::cout << "archiver -d archive_name - unzip files from the archive_name archive and put them in the current "
                     "directory."
                  << std::endl;
        std::cout << "archiver -h - display help on using the program." << std::endl;
        return 0;
    }

    return 111;
}
