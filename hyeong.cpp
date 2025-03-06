#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Casting.h>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <iostream>

using namespace llvm;

struct Token {
    std::string value;
    enum Type { COMMAND, DOTS, HEART, QUESTION, EXCLAMATION } type;
};

struct ASTNode {
    std::string command;
    int letterCount;
    int dotCount;
    std::string zone;
    int index;
};

char32_t decodeUTF8(const std::string& str, size_t& pos) {
    if (pos >= str.length()) return 0;
    unsigned char c = str[pos++];
    if (c < 0x80) return c;
    if ((c & 0xE0) == 0xC0) return ((c & 0x1F) << 6) | (str[pos++] & 0x3F);
    if ((c & 0xF0) == 0xE0) {
        char32_t result = ((c & 0x0F) << 12) | ((str[pos++] & 0x3F) << 6);
        return result | (str[pos++] & 0x3F);
    }
    return 0;
}

bool isHangul(char32_t c) {
    return c >= 0xAC00 && c <= 0xD7A3;
}

std::vector<Token> tokenize(const std::string& source) {
    std::vector<Token> tokens;
    std::string current;
    bool inCommand = false;
    size_t pos = 0;

    while (pos < source.length()) {
        char32_t c = decodeUTF8(source, pos);
        if (isHangul(c)) {
            if (!inCommand && !current.empty()) {
                tokens.push_back({current, Token::DOTS});
                current.clear();
            }
            inCommand = true;
            current += source.substr(pos - (c < 0x80 ? 1 : c < 0x800 ? 2 : 3), c < 0x80 ? 1 : c < 0x800 ? 2 : 3);
        } else if (c == '.' || c == 0x2026 || c == 0x22EF || c == 0x22EE) {
            if (inCommand) {
                tokens.push_back({current, Token::COMMAND});
                current.clear();
                inCommand = false;
            }
            current += (c == '.') ? "." : "...";
        } else if (c == 0x2665 || c == 0x2661 || c == '?' || c == '!') {
            if (!current.empty()) {
                tokens.push_back({current, inCommand ? Token::COMMAND : Token::DOTS});
                current.clear();
            }
            inCommand = false;
            if (c == '?') tokens.push_back({"?", Token::QUESTION});
            else if (c == '!') tokens.push_back({"!", Token::EXCLAMATION});
            else tokens.push_back({std::string(1, c == 0x2665 ? 'H' : 'L'), Token::HEART});
        }
    }
    if (!current.empty()) {
        tokens.push_back({current, inCommand ? Token::COMMAND : Token::DOTS});
    }
    return tokens;
}

std::vector<ASTNode> parse(const std::vector<Token>& tokens) {
    std::vector<ASTNode> ast;
    std::string zone;
    int index = 0;

    for (size_t i = 0; i < tokens.size(); i++) {
        if (tokens[i].type == Token::COMMAND) {
            std::string cmd = tokens[i].value;
            int dots = 0;
            if (i + 1 < tokens.size() && tokens[i + 1].type == Token::DOTS) {
                dots = tokens[i + 1].value.length();
                i++;
            }
            while (i + 1 < tokens.size() && tokens[i + 1].type != Token::COMMAND) {
                zone += tokens[i + 1].value;
                i++;
            }
            int letters = 0;
            size_t pos = 0;
            while (pos < cmd.length()) {
                if (isHangul(decodeUTF8(cmd, pos))) letters++;
            }
            ast.push_back({cmd, letters, dots, zone, index++});
            zone.clear();
        }
    }
    return ast;
}

class HyeongCompiler {
    LLVMContext context;
    Module* module;
    IRBuilder<> builder;
    std::vector<Value*> stacks;
    std::vector<Value*> stackPointers;
    int currentStack = 3;
    std::map<std::pair<int, std::string>, int> heartMap;
    std::vector<int> jumpStack;
    std::vector<BasicBlock*> blocks;
    Function* putcharFunc;
    Type* Int32Ty;

    Value* popStack(int stackIdx) {
        Value* sp = builder.CreateLoad(Int32Ty, stackPointers[stackIdx]);
        Value* newSp = builder.CreateSub(sp, builder.getInt32(1));
        builder.CreateStore(newSp, stackPointers[stackIdx]);
        Value* ptr = builder.CreateGEP(Int32Ty, stacks[stackIdx], newSp);
        return builder.CreateLoad(Int32Ty, ptr);
    }

    void pushStack(int stackIdx, Value* value) {
        Value* sp = builder.CreateLoad(Int32Ty, stackPointers[stackIdx]);
        Value* ptr = builder.CreateGEP(Int32Ty, stacks[stackIdx], sp);
        builder.CreateStore(value, ptr);
        if (stackIdx == 1) { // stdout
            builder.CreateCall(putcharFunc, value);
        }
        builder.CreateStore(builder.CreateAdd(sp, builder.getInt32(1)), stackPointers[stackIdx]);
    }

public:
    HyeongCompiler() : builder(context) {
        module = new Module("hyeong", context);
        InitializeNativeTarget();
        auto putcharDecl = module->getOrInsertFunction("putchar", FunctionType::get(builder.getInt32Ty(), {builder.getInt32Ty()}, false));
        putcharFunc = cast<Function>(putcharDecl.getCallee());
        Int32Ty = builder.getInt32Ty();
    }

    void compile(const std::vector<ASTNode>& ast) {
        FunctionType* funcType = FunctionType::get(Int32Ty, false);
        Function* mainFunc = Function::Create(funcType, Function::ExternalLinkage, "main", module);
        
        blocks.resize(ast.size() + 1);
        for (size_t i = 0; i <= ast.size(); i++) {
            blocks[i] = BasicBlock::Create(context, "cmd" + std::to_string(i), mainFunc);
        }
        
        builder.SetInsertPoint(blocks[0]);
        for (int i = 0; i < 4; i++) {
            Value* stack = builder.CreateAlloca(ArrayType::get(Int32Ty, 100));
            Value* sp = builder.CreateAlloca(Int32Ty);
            builder.CreateStore(builder.getInt32(0), sp);
            stacks.push_back(stack);
            stackPointers.push_back(sp);
        }

        for (size_t i = 0; i < ast.size(); i++) {
            builder.SetInsertPoint(blocks[i]);
            const ASTNode& node = ast[i];

            if (node.command == "형" || (node.command.find("혀") == 0 && node.command.find("엉") != std::string::npos)) {
                int value = node.letterCount * node.dotCount;
                pushStack(currentStack, builder.getInt32(value));
            }
            else if (node.command == "항" || (node.command.find("하") == 0 && node.command.find("앙") != std::string::npos)) {
                Value* sum = builder.getInt32(0);
                for (int j = 0; j < node.letterCount; j++) {
                    sum = builder.CreateAdd(sum, popStack(currentStack));
                }
                pushStack(node.dotCount, sum);
            }
            else if (node.command == "핫" || (node.command.find("하") == 0 && node.command.find("앗") != std::string::npos)) {
                Value* prod = builder.getInt32(1);
                for (int j = 0; j < node.letterCount; j++) {
                    prod = builder.CreateMul(prod, popStack(currentStack));
                }
                pushStack(node.dotCount, prod);
            }
            else if (node.command == "흣" || (node.command.find("흐") == 0 && node.command.find("읏") != std::string::npos)) {
                std::vector<Value*> values;
                for (int j = 0; j < node.letterCount; j++) {
                    values.push_back(popStack(currentStack));
                }
                Value* sum = builder.getInt32(0);
                for (int j = node.letterCount - 1; j >= 0; j--) {
                    Value* neg = builder.CreateNeg(values[j]);
                    pushStack(currentStack, neg);
                    sum = builder.CreateAdd(sum, neg);
                }
                pushStack(node.dotCount, sum);
            }
            else if (node.command == "흡" || (node.command.find("흐") == 0 && node.command.find("읍") != std::string::npos)) {
                std::vector<Value*> values;
                for (int j = 0; j < node.letterCount; j++) {
                    values.push_back(popStack(currentStack));
                }
                Value* prod = builder.getInt32(1);
                for (int j = node.letterCount - 1; j >= 0; j--) {
                    Value* recip = builder.CreateSDiv(builder.getInt32(1), values[j]);
                    pushStack(currentStack, recip);
                    prod = builder.CreateMul(prod, recip);
                }
                pushStack(node.dotCount, prod);
            }
            else if (node.command == "흑" || (node.command.find("흐") == 0 && node.command.find("윽") != std::string::npos)) {
                Value* val = popStack(currentStack);
                for (int j = 0; j < node.letterCount; j++) {
                    pushStack(node.dotCount, val);
                }
                pushStack(currentStack, val);
                currentStack = node.dotCount;
            }
            else if (!node.zone.empty()) {
                std::string heart;
                std::vector<std::string> zones;
                std::string currentZone;
                for (char c : node.zone) {
                    if (c == '?' || c == '!') {
                        if (!currentZone.empty()) zones.push_back(currentZone);
                        currentZone.clear();
                        zones.push_back(std::string(1, c));
                    } else {
                        currentZone += c;
                    }
                }
                if (!currentZone.empty()) zones.push_back(currentZone);

                if (zones.size() == 1 && zones[0] == "L") { // ♡
                    if (!jumpStack.empty()) {
                        builder.CreateBr(blocks[jumpStack.back()]);
                        continue;
                    }
                } else if (zones.size() == 1 && zones[0] == "H") { // ♥
                    int key = node.letterCount * node.dotCount;
                    if (heartMap.count({key, "H"}) == 0) {
                        heartMap[{key, "H"}] = i;
                    } else {
                        jumpStack.push_back(i);
                        builder.CreateBr(blocks[heartMap[{key, "H"}]]);
                        continue;
                    }
                } else if (zones.size() >= 3) {
                    Value* val = popStack(currentStack);
                    int threshold = node.letterCount * node.dotCount;
                    BasicBlock* thenBlock = BasicBlock::Create(context, "then", mainFunc);
                    BasicBlock* elseBlock = BasicBlock::Create(context, "else", mainFunc);
                    Value* cond = (zones[1] == "?") ? builder.CreateICmpSLT(val, builder.getInt32(threshold)) : builder.CreateICmpEQ(val, builder.getInt32(threshold));
                    builder.CreateCondBr(cond, thenBlock, elseBlock);

                    builder.SetInsertPoint(thenBlock);
                    builder.CreateBr(blocks[heartMap[{node.letterCount * node.dotCount, zones[0]}]]);
                    builder.SetInsertPoint(elseBlock);
                    builder.CreateBr(blocks[heartMap[{node.letterCount * node.dotCount, zones[2]}]]);
                    continue;
                }
            }

            builder.CreateBr(blocks[i + 1]);
        }

        builder.SetInsertPoint(blocks[ast.size()]);
        builder.CreateRet(builder.getInt32(0));
    }

    void saveIR(const std::string& filename) {
        std::error_code EC;
        raw_fd_ostream out(filename, EC);
        module->print(out, nullptr);
    }

    ~HyeongCompiler() {
        delete module;
    }
};

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <source.hyeong>" << std::endl;
        return 1;
    }
    std::ifstream file(argv[1]);
    std::string source((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    auto tokens = tokenize(source);
    auto ast = parse(tokens);
    HyeongCompiler compiler;
    compiler.compile(ast);
    compiler.saveIR("output.ll");
    return 0;
}
