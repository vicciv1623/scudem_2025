#include <iostream>
#include <utility>
#include <cmath>
#include <algorithm>
#include <fstream>

using namespace std;

ofstream file;

float dxdt(float x){
    return 0.1*x;
}

void adamsBashforth(pair<float, float>& p, float step, int tStart, int n){
    for(int i=1; i<n; i++){
        float t = tStart + step*i;
        float updateP = p.second + step/2 * (3*dxdt(p.second) - dxdt(p.first));
        
        p.first = p.second;
        p.second = updateP;

        file<<t<<","<<p.second<<endl;
    }
}

int main(){
    int n = 60;
    int tStart = 2000;
    int tEnd = 2020;
    float step = (tEnd-tStart)/float(n);

    pair<float, float> p;
    p.first = 6;
    p.second = p.first + step/2*(3*dxdt(p.first));

    file.open("results.txt");
    file<<"time,value"<<endl;
    file<<tStart<<","<<p.first<<endl;
    tStart+=step;
    file<<tStart<<","<<p.second<<endl;

    adamsBashforth(p, step, tStart, n);
    file.close();

    return 0;
}