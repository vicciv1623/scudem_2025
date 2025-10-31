#include <iostream>
#include <utility>
#include <cmath>
#include <algorithm>
#include <fstream>
#include <random>
#include <numbers>
#include <array>

using namespace std;

//these are most commonly used parameters
namespace params{
    ofstream fileV;
    ofstream fileP;
    ofstream fileM;
    ofstream fileO;

    const int dustDensity=2e3;       //2000 kg/m^3
    const float alpha=0.5;
    const float beta=1.0;
    const float gamma=1;
    const float terminalVelocity=70;

    float radius;                   //m
    float temp;                     //kelvin
    float altitude;                 //km
    float airViscosity;
    float diffusionCoeff;
    float airDensity;
    
    unsigned seed=time(0);
    default_random_engine generator(seed);
}

//this indicates coordinates for position of particle
struct pos3{
    float x;
    float y;
    float z;

    pos3 operator+(const pos3& other) const{
        return {x + other.x, y + other.y, z + other.z};
    }
    pos3 operator-(const pos3& other) const{
        return {x - other.x, y - other.y, z - other.z};
    }
    pos3 operator*(float s) const{
        return {x*s, y*s, z*s};
    }
    void factor(float f){
        float sum = sqrt(pow(x,2) + pow(y,2) + pow(z,2));
        x*=f/sum;
        y*=f/sum;
        z*=f/sum;
    }
    void print(){
        params::fileP<<x<<"-"<<y<<"-"<<z<<",";
    }
};

float dragCoeff(){
    return 0.5;
}
float surfaceArea(){
    //surface area of a sphere
    return 4 * M_PI * pow(params::radius, 2);
}
void upRadius(float mass){
    //volume of a sphere
    float volume = mass/params::dustDensity;
    params::radius = cbrt(0.75 * volume / M_PI);
}
void upAltitude(pos3 p){
    params::altitude -= p.z;
}
void upAirViscosity(){
    //Sutherland's law
    params::airViscosity = 1.716e-5 * pow(params::temp/273, 1.5) * 384 / (params::temp + 111);
}
void upDiffusionCoeff(){
    //Stoke-Einstein equation
    params::diffusionCoeff = 1.38e-23 * params::temp / (6 * M_PI * params::airViscosity * params::radius);
}
//void upTemp(){}
void upAirDensity(){
    //https://www.grc.nasa.gov/www/k-12/airplane/atmosmet.html
    //https://www.eoas.ubc.ca/courses/atsc113/flying/met_concepts/02-met_concepts/02a-std_atmos-P/index.html

    //trophosphere
    float pressure;
    if(params::altitude < 11000){
        pressure = 101.29 * pow(params::temp/288.08, 5.256);
    }
    //lower stratosphere
    else if(params::altitude < 25000){
        pressure = 22.65 * exp(1.73 - 0.000157*params::altitude);
    }
    //upper stratosphere
    else{
        pressure = 2.488 / pow(params::temp/216.6, 11.388);
        cout<<pressure<<endl;
    }

    params::airDensity = pressure / (0.2869 * params::temp);
}

//differential equations
//dvdt is km/s
float dvdt(float v, float mass){
    return -params::alpha*(0.5 * params::airDensity * pow(v, 2) * dragCoeff() * surfaceArea() + mass*9.81);
}

//normal_distribution(mean, stdv)
pos3 dpdt(float velocity){
    normal_distribution<float> dist(0, 2*params::diffusionCoeff);
    pos3 r;
    r.x = dist(params::generator);
    r.y = dist(params::generator);
    r.z = abs(dist(params::generator));

    r.factor(velocity);

    //cout<<r.x<<" "<<r.y<<" "<<r.z<<endl;
    return r;
}

float dmdt(float mass){
    return -3*mass / params::radius * params::gamma; //add drdt??
}

void updateParams(float mass, pos3 p){
    upRadius(mass);
    upAltitude(p);
    //upTemp
    upAirViscosity();
    upDiffusionCoeff();
    upAirDensity();
}

void particle(int n, vector<vector<float>>& velocity, vector<vector<pos3>>& position, vector<vector<float>>& mass){
    gamma_distribution<float> initVel(15, 0.9);     //this is in km
    normal_distribution<float> initMass(0.00001, 0.000003);     //this is in kg

    float prevV;
    pos3 prevP;
    float prevM;

    float currV;
    pos3 currP;
    float currM;

    for(int i=0; i<n; i++){
        //first values
        velocity[i].push_back(abs(initVel(params::generator)));
        position[i].push_back({0, 0, 0});
        mass[i].push_back(abs(initMass(params::generator)));
        params::altitude = 100;

        currV = velocity[i][0];
        currP = position[i][0];
        currM = mass[i][0];
        updateParams(currM, currP);

        //second values
        velocity[i].push_back(currV + 1.5*dvdt(currV, currM));
        position[i].push_back(currP + dpdt(currV)*1.5);
        //mass

        prevV = currV;
        prevP = currP;
        prevM = currM;

        currV = velocity[i][1];
        currP = position[i][1];
        currM = mass[i][1];
        updateParams(currM, currP);

        //rest of the model
        while(params::altitude > 0){
            //adamsbashforth
            velocity[i].push_back(currV + 0.5*(3*dvdt(currV, currM) - dvdt(prevV, prevM)));
            position[i].push_back(currP + (dpdt(currV)*3 - dpdt(prevV))*0.5);
            //mass

            prevV = currV;
            prevP = currP;
            prevM = currM;

            currV = velocity[i].back();
            currP = position[i].back();
            currM = mass[i].back();

            if(currM < 0){
                break;
            }

            updateParams(currM, currP);
        }
    }
}

//user read_csv from readr package in R to read these txt files
void parseData(vector<vector<float>>& data, ofstream& f){
    for(int i=0; i<data.size(); i++){
        f<<i<<",";
        for(int j=0; j<data[0].size(); j++){
            f<<data[i][j]<<",";
        }
        f<<"\n";
    }
}
void parseData(vector<vector<pos3>>& data){
    for(int i=0; i<data.size(); i++){
        params::fileP<<i<<",";
        for(int j=0; j<data[0].size(); j++){
            data[i][j].print();
        }
        params::fileP<<"\n";
    }
}

int main(){
    int n=1;
    vector<vector<float>> velocity(n);
    vector<vector<pos3>> position(n);
    vector<vector<float>> mass(n);

    particle(n, velocity, position, mass);

    params::fileV.open("results/velocity.txt");
    params::fileP.open("results/position.txt");
    params::fileM.open("results/mass.txt");
    parseData(velocity, params::fileV);
    parseData(position);
    parseData(mass, params::fileM);
    params::fileV.close();
    params::fileP.close();
    params::fileM.close();

    return 0;
}