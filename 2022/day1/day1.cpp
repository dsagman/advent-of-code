#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

int main()
{
    std::vector<std::string> fileLines;
    std::ifstream in("input1.txt");
    std::string aLine;
    while (std::getline(in, aLine))
    {
        fileLines.push_back(aLine);
    }
    int runningSum = 0;
    std::vector<int> sumVec;
    for (std::string v : fileLines)
    {
        if (v == "")
        {
            sumVec.push_back(runningSum);
            runningSum = 0;
        }
        else
        {
            runningSum += std::stoi(v);
        }
    }
    sumVec.push_back(runningSum);
    std::cout << "Part 1 answer: ";
    std::cout << *max_element(std::begin(sumVec), std::end(sumVec));
    std::cout << '\n';

    std::sort(sumVec.begin(), sumVec.end(), std::greater<int>());
    std::cout << "Part 2 answer: ";
    std::cout << std::accumulate(sumVec.begin(), sumVec.begin() + 3, 0);
    std::cout << '\n';
}