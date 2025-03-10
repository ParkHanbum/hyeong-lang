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
#include <utility>

using namespace llvm;

struct Token
{
    std::string value;
    enum Type
    {
        COMMAND,
        DOTS,
        HEART,
        QUESTION,
        EXCLAMATION
    } type;
};

struct ASTNode
{
    std::string command;
    int letterCount;
    int dotCount;
    std::string zone;
    int index;
};

char32_t decodeUTF8(const std::string &str, size_t &pos)
{
    if (pos >= str.length())
        return 0;
    unsigned char c = str[pos++];
    if (c < 0x80)
        return c;
    if ((c & 0xE0) == 0xC0)
        return ((c & 0x1F) << 6) | (str[pos++] & 0x3F);
    if ((c & 0xF0) == 0xE0)
    {
        char32_t result = ((c & 0x0F) << 12) | ((str[pos++] & 0x3F) << 6);
        return result | (str[pos++] & 0x3F);
    }
    if ((c & 0xF8) == 0xF0)
    {
        char32_t result = ((c & 0x07) << 18) | ((str[pos++] & 0x3F) << 12);
        result |= ((str[pos++] & 0x3F) << 6);
        return result | (str[pos++] & 0x3F);
    }
    return 0;
}

bool isHangul(char32_t c)
{
    return c >= 0xAC00 && c <= 0xD7A3;
}

bool isHeart(char32_t c)
{
    return c == 0x2665 || c == 0x2661 || c == 0x2764 || c == 0x1F495 || c == 0x1F496 ||
           c == 0x1F497 || c == 0x1F498 || c == 0x1F499 || c == 0x1F49A || c == 0x1F49B ||
           c == 0x1F49C || c == 0x1F49D;
}

std::vector<Token> tokenize(const std::string &source)
{
    std::vector<Token> tokens;
    std::string current;
    bool inCommand = false;
    size_t pos = 0;

    while (pos < source.length())
    {
        size_t oldPos = pos;
        char32_t c = decodeUTF8(source, pos);
        if (isHangul(c))
        {
            if (!inCommand && !current.empty())
            {
                tokens.push_back({current, Token::DOTS});
                current.clear();
            }
            inCommand = true;
            size_t len = pos - oldPos;
            current += source.substr(oldPos, len);
        }
        else if (c == '.' || c == 0x2026 || c == 0x22EF || c == 0x22EE)
        {
            if (inCommand)
            {
                tokens.push_back({current, Token::COMMAND});
                current.clear();
                inCommand = false;
            }
            current += (c == '.') ? "." : "...";
        }
        else if (isHeart(c) || c == '?' || c == '!')
        {
            if (!current.empty())
            {
                tokens.push_back({current, inCommand ? Token::COMMAND : Token::DOTS});
                current.clear();
            }
            inCommand = false;
            if (c == '?')
            {
                tokens.push_back({"?", Token::QUESTION});
            }
            else if (c == '!')
            {
                tokens.push_back({"!", Token::EXCLAMATION});
            }
            else
            {
                size_t heartLen = (c < 0x80 ? 1 : (c < 0x800 ? 2 : (c < 0x10000 ? 3 : 4)));
                tokens.push_back({source.substr(oldPos, heartLen), Token::HEART});
            }
        }
    }
    if (!current.empty())
    {
        tokens.push_back({current, inCommand ? Token::COMMAND : Token::DOTS});
    }
    return tokens;
}

std::vector<ASTNode> parse(const std::vector<Token> &tokens)
{
    std::vector<ASTNode> ast;
    std::string zone;
    int index = 0;

    for (size_t i = 0; i < tokens.size(); i++)
    {
        if (tokens[i].type == Token::COMMAND)
        {
            std::string cmd = tokens[i].value;
            int dots = 0;
            if (i + 1 < tokens.size() && tokens[i + 1].type == Token::DOTS)
            {
                dots = tokens[i + 1].value.length();
                i++;
            }
            while (i + 1 < tokens.size() && tokens[i + 1].type != Token::COMMAND)
            {
                zone += tokens[i + 1].value;
                i++;
            }
            int letters = 0;
            size_t pos = 0;
            while (pos < cmd.length())
            {
                if (isHangul(decodeUTF8(cmd, pos)))
                    letters++;
            }
            ast.push_back({cmd, letters, dots, zone, index++});
            zone.clear();
        }
    }

    for (const auto &node : ast)
    {
        std::cout << "Command: " << node.command << ", Letters: " << node.letterCount
                  << ", Dots: " << node.dotCount << ", Zone: " << node.zone << "\n";
    }
    return ast;
}

bool isHeartSymbol(const std::string &s)
{
    return s == u8"\u2665" || s == u8"\u2764" || s == u8"\U0001F495" ||
           s == u8"\U0001F496" || s == u8"\U0001F497" || s == u8"\U0001F498" ||
           s == u8"\U0001F499" || s == u8"\U0001F49A" || s == u8"\U0001F49B" ||
           s == u8"\U0001F49C" || s == u8"\U0001F49D";
}

class HyeongCompiler
{
    LLVMContext context;
    Module *module;
    IRBuilder<> builder;
    std::vector<Value *> stacks;
    std::vector<Value *> stackPointers;
    int currentStack = 3;
    std::map<std::pair<int, std::string>, int> heartMap;
    std::vector<int> jumpStack;
    std::vector<BasicBlock *> blocks;
    Function *putcharFunc;
    Function *printfFunc; // 디버깅용 printf 추가
    Type *Int32Ty;

    void ensureStack(int idx)
    {
        while (stacks.size() <= (size_t)idx)
        {
            Value *stack = builder.CreateAlloca(ArrayType::get(Int32Ty, 100), nullptr, "stack_dyn");
            Value *sp = builder.CreateAlloca(Int32Ty, nullptr, "sp_dyn");
            builder.CreateStore(builder.getInt32(0), sp);
            builder.CreateMemSet(stack, builder.getInt8(0), builder.getInt64(400), MaybeAlign());
            stacks.push_back(stack);
            stackPointers.push_back(sp);
        }
    }

    Value *popStack(int stackIdx)
    {
        ensureStack(stackIdx);
        Value *sp = builder.CreateLoad(Int32Ty, stackPointers[stackIdx]);
        Value *cond = builder.CreateICmpSLE(sp, builder.getInt32(0));
        Value *newSp = builder.CreateSub(sp, builder.getInt32(1));
        Value *selectedSp = builder.CreateSelect(cond, builder.getInt32(0), newSp);
        builder.CreateStore(selectedSp, stackPointers[stackIdx]);
        Value *ptr = builder.CreateGEP(Int32Ty, stacks[stackIdx], selectedSp);
        Value *val = builder.CreateLoad(Int32Ty, ptr);
        Value *result = builder.CreateSelect(cond, builder.getInt32(1), val);

        // 디버깅: pop된 값 출력
        Constant *formatStr = builder.CreateGlobalStringPtr("\nPop from stack %d: %d, SP: %d");
        builder.CreateCall(printfFunc, {formatStr, builder.getInt32(stackIdx), result, sp});
        return result;
    }

    void pushStack(int stackIdx, Value *value)
    {
        ensureStack(stackIdx);
        Value *sp = builder.CreateLoad(Int32Ty, stackPointers[stackIdx]);
        Constant *formatStr = builder.CreateGlobalStringPtr("\nPush to stack %d: %d, SP: %d");
        builder.CreateCall(printfFunc, {formatStr, builder.getInt32(stackIdx), value, sp});
        Value *cond = builder.CreateICmpSGE(sp, builder.getInt32(100));
        Value *selectedSp = builder.CreateSelect(cond, builder.getInt32(99), sp);
        Value *ptr = builder.CreateGEP(Int32Ty, stacks[stackIdx], selectedSp);
        builder.CreateStore(value, ptr);

        if (stackIdx == 1)
        { // stdout 스택일 때만 출력
            builder.CreateCall(putcharFunc, value);
        }
        Value *newSp = builder.CreateAdd(selectedSp, builder.getInt32(1));
        Value *cappedSp = builder.CreateSelect(cond, builder.getInt32(100), newSp);
        builder.CreateStore(cappedSp, stackPointers[stackIdx]);
    }

public:
    HyeongCompiler() : builder(context)
    {
        module = new Module("hyeong", context);
        InitializeNativeTarget();
        auto putcharDecl = module->getOrInsertFunction("putchar",
                                                       FunctionType::get(builder.getInt32Ty(), {builder.getInt32Ty()}, false));
        putcharFunc = cast<Function>(putcharDecl.getCallee());

        // printf 함수 선언 수정: i8* 타입 사용
        FunctionType *printfTy = FunctionType::get(builder.getInt32Ty(), {builder.getInt8Ty()->getPointerTo()}, true);
        printfFunc = Function::Create(printfTy, Function::ExternalLinkage, "printf", module);

        Int32Ty = builder.getInt32Ty();
    }

    void compile(const std::vector<ASTNode> &ast)
    {
        FunctionType *funcType = FunctionType::get(Int32Ty, false);
        Function *mainFunc = Function::Create(funcType, Function::ExternalLinkage, "main", module);

        BasicBlock *entryBlock = BasicBlock::Create(context, "entry", mainFunc);
        builder.SetInsertPoint(entryBlock);
        for (int i = 0; i < 4; i++)
        {
            Value *stack = builder.CreateAlloca(ArrayType::get(Int32Ty, 100), nullptr, "stack" + std::to_string(i));
            Value *sp = builder.CreateAlloca(Int32Ty, nullptr, "sp" + std::to_string(i));
            builder.CreateStore(builder.getInt32(0), sp);
            builder.CreateMemSet(stack, builder.getInt8(0), builder.getInt64(400), MaybeAlign());
            stacks.push_back(stack);
            stackPointers.push_back(sp);
        }
        BasicBlock *firstCmd = BasicBlock::Create(context, "cmd0", mainFunc);
        builder.CreateBr(firstCmd);

        blocks.resize(ast.size() + 1);
        blocks[0] = firstCmd;
        for (size_t i = 1; i <= ast.size(); i++)
        {
            blocks[i] = BasicBlock::Create(context, "cmd" + std::to_string(i), mainFunc);
        }

        for (size_t i = 0; i < ast.size(); i++)
        {
            builder.SetInsertPoint(blocks[i]);
            const ASTNode &node = ast[i];

            if (node.command == "형" || (node.command.find("혀") == 0 && node.command.find("엉") != std::string::npos))
            {
                int value = node.letterCount * node.dotCount;
                pushStack(currentStack, builder.getInt32(value));
            }
            else if (node.command == "항" || (node.command.find("하") == 0 && node.command.find("앙") != std::string::npos))
            {
                Value *sum = builder.getInt32(0);
                for (int j = 0; j < node.letterCount; j++)
                {
                    sum = builder.CreateAdd(sum, popStack(currentStack));
                }
                pushStack(node.dotCount, sum);
            }
            else if (node.command == "핫" || (node.command.find("하") == 0 && node.command.find("앗") != std::string::npos))
            {
                Value *prod = builder.getInt32(1);
                for (int j = 0; j < node.letterCount; j++)
                {
                    prod = builder.CreateMul(prod, popStack(currentStack));
                }
                pushStack(node.dotCount, prod);
            }
            else if (node.command == "흣" || (node.command.find("흐") == 0 && node.command.find("읏") != std::string::npos))
            {
                std::vector<Value *> values;
                for (int j = 0; j < node.letterCount; j++)
                {
                    values.push_back(popStack(currentStack));
                }
                Value *sum = builder.getInt32(0);
                for (int j = node.letterCount - 1; j >= 0; j--)
                {
                    Value *neg = builder.CreateNeg(values[j]);
                    pushStack(currentStack, neg);
                    sum = builder.CreateAdd(sum, neg);
                }
                pushStack(node.dotCount, sum);
            }
            else if (node.command == "흡" || (node.command.find("흐") == 0 && node.command.find("읍") != std::string::npos))
            {
                std::vector<Value *> values;
                for (int j = 0; j < node.letterCount; j++)
                {
                    values.push_back(popStack(currentStack));
                }
                Value *prod = builder.getInt32(1);
                for (int j = node.letterCount - 1; j >= 0; j--)
                {
                    Value *recip = builder.CreateSDiv(builder.getInt32(1), values[j]);
                    pushStack(currentStack, recip);
                    prod = builder.CreateMul(prod, recip);
                }
                pushStack(node.dotCount, prod);
            }
            else if (node.command == "흑" || (node.command.find("흐") == 0 && node.command.find("윽") != std::string::npos))
            {
                Value *val = popStack(currentStack);
                for (int j = 0; j < node.letterCount; j++)
                {
                    pushStack(node.dotCount, val);
                }
                pushStack(currentStack, val);
                currentStack = node.dotCount;
            }
            else if (!node.zone.empty())
            {
                std::vector<std::string> zones;
                std::string currentZone;
                for (char c : node.zone)
                {
                    if (c == '?' || c == '!')
                    {
                        if (!currentZone.empty())
                            zones.push_back(currentZone);
                        currentZone.clear();
                        zones.push_back(std::string(1, c));
                    }
                    else
                    {
                        currentZone += c;
                    }
                }
                if (!currentZone.empty())
                    zones.push_back(currentZone);

                if (zones.size() == 1)
                {
                    std::string heartSymbol = zones[0];
                    if (heartSymbol == u8"\u2661")
                    {
                        if (!jumpStack.empty())
                        {
                            builder.CreateBr(blocks[jumpStack.back()]);
                            continue;
                        }
                    }
                    else if (isHeartSymbol(heartSymbol))
                    {
                        int key = node.letterCount * node.dotCount;
                        std::pair<int, std::string> keyPair = {key, heartSymbol};
                        if (heartMap.count(keyPair) == 0)
                        {
                            heartMap[keyPair] = i;
                        }
                        else
                        {
                            if (heartMap[keyPair] != i)
                            {
                                jumpStack.push_back(i);
                                builder.CreateBr(blocks[heartMap[keyPair]]);
                                continue;
                            }
                        }
                    }
                }
                else if (zones.size() >= 3 && (zones[1] == "?" || zones[1] == "!"))
                {
                    Value *val = popStack(currentStack);
                    int threshold = node.letterCount * node.dotCount;
                    BasicBlock *thenBlock = BasicBlock::Create(context, "then", mainFunc);
                    BasicBlock *elseBlock = BasicBlock::Create(context, "else", mainFunc);
                    Value *cond = (zones[1] == "?") ? builder.CreateICmpSLT(val, builder.getInt32(threshold))
                                                    : builder.CreateICmpEQ(val, builder.getInt32(threshold));
                    builder.CreateCondBr(cond, thenBlock, elseBlock);

                    builder.SetInsertPoint(thenBlock);
                    std::pair<int, std::string> thenKey = {node.letterCount * node.dotCount, zones[0]};
                    if (heartMap.count(thenKey) == 0)
                    {
                        heartMap[thenKey] = i;
                    }
                    builder.CreateBr(blocks[heartMap[thenKey]]);

                    builder.SetInsertPoint(elseBlock);
                    std::pair<int, std::string> elseKey = {node.letterCount * node.dotCount, zones[2]};
                    if (heartMap.count(elseKey) == 0)
                    {
                        heartMap[elseKey] = i;
                    }
                    builder.CreateBr(blocks[heartMap[elseKey]]);

                    builder.SetInsertPoint(blocks[i + 1]);
                    continue;
                }
            }

            builder.CreateBr(blocks[i + 1]);
        }

        builder.SetInsertPoint(blocks[ast.size()]);
        builder.CreateRet(builder.getInt32(0));
    }

    void saveIR(const std::string &filename)
    {
        std::error_code EC;
        raw_fd_ostream out(filename, EC);
        module->print(out, nullptr);
    }

    ~HyeongCompiler()
    {
        delete module;
    }
};

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
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
