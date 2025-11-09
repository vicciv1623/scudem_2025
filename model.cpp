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

    const int dustDensity=600;       //2000 kg/m^3
    const float alpha=8e10;
    //const float alpha=8e11;
    const float gamma=1e-9;
    const float terminalVelocity=70;

    float radius;                   //m
    float temp;                     //kelvin
    float altitude;                 //km
    float airViscosity;
    float diffusionCoeff;
    float airDensity;
    
    //unsigned seed=time(0);
    unsigned seed=1223;
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
        params::fileP<<x<<":"<<y<<":"<<z<<",";
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
void upAltitude(pos3 diff){
    params::altitude -= diff.z;
}
void upAirViscosity(){
    //Sutherland's law
    //https://doc.comsol.com/5.5/doc/com.comsol.help.cfd/cfd_ug_fluidflow_high_mach.08.27.html
    params::airViscosity = 1.716e-5 * pow(params::temp/273, 1.5) * 384 / (params::temp + 111);
}
void upDiffusionCoeff(){
    //Stoke-Einstein equation
    params::diffusionCoeff = 1.38e-23 * params::temp / (6 * M_PI * params::airViscosity * params::radius);
}
void upAirDensity(){
    //https://www.grc.nasa.gov/www/k-12/airplane/atmosmet.html
    //https://www.eoas.ubc.ca/courses/atsc113/flying/met_concepts/02-met_concepts/02a-std_atmos-P/index.html

    //trophosphere
    float pressure;
    if(params::altitude <= 12){
        //params::temp = 15.04 - 6.49 * params::altitude + 273.15;
        params::temp = 288.15 - 6.25*params::altitude;
        pressure = 101.29 * pow(params::temp/288.08, 5.256);
    }
    //lower stratosphere
    else if(params::altitude < 25){
        params::temp = -56.46 + 273.15;
        pressure = 22.65 * exp(1.73 - 0.157*params::altitude);
    }
    else if(params::altitude < 50){
        params::temp = 194.2 + 30*params::altitude/19;
        pressure = 22.65 * exp(1.73 - 0.157*params::altitude);
    }
    //upper stratosphere
    else if(params::altitude < 80){
        params::temp = 423.15 - 3*params::altitude;
        pressure = 2.488 / pow(params::temp/216.6, 11.388);
    }
    else{
        params::temp = 2.56*params::altitude - 22;
        pressure = 2.488 / pow(params::temp/216.6, 11.388);
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

float dmdt(float mass, float velocity){
    return -3*mass / params::radius * params::gamma; //add drdt??
    //return -3*mass / params::radius * 3.45e-1 * params::airDensity * pow(velocity,3);
}

void updateParams(float mass, pos3 p){
    upRadius(mass);
    upAltitude(p);
    upAirDensity();
    upAirViscosity();
    upDiffusionCoeff();
}

void parseParams(){
    params::fileO<<params::radius<<":"<<params::altitude<<":"<<params::temp<<":"
    <<params::airDensity<<":"<<params::diffusionCoeff<<":"<<params::airViscosity<<",";
}

void printParams(){
    cout<<"Printing parameters: "<<params::radius<<" "<<params::temp<<" "<<
    params::altitude<<" "<<params::airViscosity<<" "<<params::diffusionCoeff<<" "<<params::airDensity<<endl;
}

void printVars(float velocity, float mass, pos3 position){
    cout<<"Printing variables: "<<velocity<<" "<<mass<<endl;
    cout<<"Printing position: "<<position.x<<" "<<position.y<<" "<<position.z<<endl;
}

void particle(int n, vector<vector<float>>& velocity, vector<vector<pos3>>& position, vector<vector<float>>& mass){
    gamma_distribution<float> initVel(15, 0.9);     //this is in km
    normal_distribution<float> initMass(2.5e-15, 1e-16);     //this is in kg

    float prevV;
    pos3 prevP;
    float prevM;

    float currV;
    pos3 currP;
    float currM;

    for(int i=0; i<n; i++){
        //first values
        params::altitude = 700;
        params::fileO<<i<<",";
        velocity[i].push_back(abs(initVel(params::generator)));
        position[i].push_back({0, 0, 0});
        mass[i].push_back(abs(initMass(params::generator)));

        currV = velocity[i][0];
        currP = position[i][0];
        currM = mass[i][0];
        updateParams(currM, currP);
        parseParams();

        //printParams();
        //printVars(currV, currM, currP);

        //second values
        velocity[i].push_back(currV + 1.5*dvdt(currV, currM));
        position[i].push_back(currP + dpdt(currV));
        mass[i].push_back(currM + 1.5*dmdt(currM, currV));

        prevV = currV;
        prevP = currP;
        prevM = currM;

        currV = velocity[i][1];
        currP = position[i][1];
        currM = mass[i][1];
        updateParams(currM, currP-prevP);
        parseParams();

        //printParams();
        //printVars(currV, currM, currP);

        //rest of the model
        while(params::altitude > 0){
            //cout<<params::altitude<<endl;
            //adamsbashforth
            velocity[i].push_back(currV + 0.5*(3*dvdt(currV, currM) - dvdt(prevV, prevM)));
            position[i].push_back(currP + dpdt(currV));
            mass[i].push_back(currM + 0.5*(3*dmdt(currM, currV) - dmdt(prevM, currV)));

            prevV = currV;
            prevP = currP;
            prevM = currM;

            currV = velocity[i].back();
            currP = position[i].back();
            currM = mass[i].back();

            if(currM < 0){
                break;
            }

            updateParams(currM, currP-prevP);
            parseParams();

            //printParams();
            //printVars(currV, currM, currP);
        }
        params::fileO<<"\n";
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
    int n=100;
    vector<vector<float>> velocity(n);
    vector<vector<pos3>> position(n);
    vector<vector<float>> mass(n);

    cout<<"running..."<<endl;
    params::fileO.open("results/other.txt");
    particle(n, velocity, position, mass);
    params::fileO.close();

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