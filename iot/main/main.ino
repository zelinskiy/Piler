#include <Stepper.h>

const int stepsPerRevolution = 300;  
const int times = 6;

Stepper myStepper(stepsPerRevolution, 8, 9, 10, 11);

void setup() {
  myStepper.setSpeed(60);
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0) {
        int incomingByte = Serial.read();
        Serial.print("I received: ");
        Serial.println(incomingByte, DEC);
        switch(incomingByte){
          case 70:
            Serial.println("forward");
            for(int i = 0; i < times; i++){    
              myStepper.step(stepsPerRevolution);     
            }
            break;
          case 66:
            Serial.println("backward");
            for(int i = 0; i < times; i++){    
              myStepper.step(-stepsPerRevolution);     
            }
            break;
        }
    }
}

