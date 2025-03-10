1. compile hyeong.cpp

clang++ -o hyeong hyeong.cpp \`llvm-config --cxxflags --ldflags --libs core\`

2. compile hyeong language source file

hyeong test.hy

3. execute lli with output.ll to see what happend

lli output.ll
