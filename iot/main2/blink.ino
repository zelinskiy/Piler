#include "HaskinoRuntime.h"

void haskinoMain();
#define HASKINOMAIN_STACK_SIZE 108
byte haskinoMainTcb[sizeof(TCB) + HASKINOMAIN_STACK_SIZE];

void setup()
    {
    haskinoMemInit();
    createTask(255, haskinoMainTcb, HASKINOMAIN_STACK_SIZE, haskinoMain);
    scheduleTask(255, 0);
    startScheduler();
    }

void loop()
    {
    }


void haskinoMain()
    {
    pinMode(10,1);
    while (1)
        {
        digitalWrite(10,1);
        delayMilliseconds(1000);
        digitalWrite(10,0);
        delayMilliseconds(1000);
        }
    taskComplete();
    }
