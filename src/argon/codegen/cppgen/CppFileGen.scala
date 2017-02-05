package argon.codegen.cppgen

import argon.codegen.FileGen
import argon.Config

trait CppFileGen extends FileGen {
  import IR._


  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def process[S:Staged](b: Block[S]): Block[S] = {

    // Forcefully create the following streams
    withStream(getStream("TopHost")) {
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Begin!")}
      preprocess(b)
      emitMain(b)
      postprocess(b)
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Complete!")}
      b
    }
  }


  override protected def emitFileHeader() {
    emit(s"""#include "interface.h"
#include <stdint.h>
#include <sys/time.h>
#include <iostream>
#include <fstream>
#include <string> 
#include <sstream> 
#include <signal.h>
#include <sys/wait.h>
#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "DeliteCpp.h"
#include "cppDeliteArraystring.h"
#include "cppDeliteArrays.h"
#include "cppDeliteArraydouble.h"

void Top_run( Interface_t *args )
{
  // int numInputs = sizeof(args->ArgIns) / sizeof(args->ArgIns[0]);
  // std::string argString = "";
  // for (int i=0; i < numInputs; i++) {
  //   argString += " ";
  //   std::ostringstream ss;
  //   ss << *args->ArgIns[i];
  //   argString += ss.str();
  // }

  // TODO: Figure out how to get Makefile to compile verilator in when we make bitstream-cpu
  // TODO: Figure out why verilator runs 1000x slower when launched from syscall
  // // std::string cmdStr = "sbt \"test:run-main app.Launcher TopModule " + argString + "\"";
  // // const char * cmd = cmdStr.c_str();
  // std::string cmdStr = "test:run-main app.Launcher TopModule " + argString;
  // char * cmd = (char*)cmdStr.c_str();
  // char *argv[] = { "sbt", cmd, 0 };

  // // Make fork with timeout
  // int timeout_time=40;
  // pid_t intermediate_pid = fork();
  // if (intermediate_pid == 0) {
  //   pid_t worker_pid = fork();
  //   if (worker_pid == 0) {
  //     execvp(argv[0], argv);
  //     printf("Simulation success!\\n");
  //       _exit(0);
  //   }

  //   pid_t timeout_pid = fork();
  //   if (timeout_pid == 0) {
  //       sleep(timeout_time);
  //       printf("============================\\n");
  //       printf("ERROR: Simulation timeout!!\\n");
  //       printf("============================\\n");
  //       _exit(0);
  //   }

  //   pid_t exited_pid = wait(NULL);
  //   if (exited_pid == worker_pid) {
  //       kill(timeout_pid, SIGKILL);
  //   } else {
  //       kill(worker_pid, SIGKILL); // Or something less violent if you prefer
  //   }
  //   wait(NULL); // Collect the other process
  //   _exit(0); // Or some more informative status
  // }
  // waitpid(intermediate_pid, 0, 0);

  // system(cmd);

  uid_t uid = geteuid();
  struct passwd *pw = getpwuid (uid);
  std::ostringstream stringStream;
  stringStream << "/tmp/chisel_test_result_" << pw->pw_name;
  std::string fname = stringStream.str();
  std::ifstream result_file;
  result_file.open( fname.c_str() );

  std::string line;
  std::vector<int64_t> results;
  int lenMemOut = 0;
  while (std::getline(result_file, line))
  {
    lenMemOut += 1;
    std::istringstream buffer(line);
    int value;
    buffer >> value;   // value = 45
    results.push_back(value);
  }

  int numArgOuts = sizeof(args->ArgOuts)/sizeof(args->ArgOuts[0]);
  if (numArgOuts > 0) {
    for ( int i = 0; i < numArgOuts; i++) {
      *args->ArgOuts[i] = results[i];
    }
  } else {
    for ( int i = 0; i < lenMemOut - 1; i++) {
      args->add_mem(results[i]);
    }    
  }

  *args->cycles = results[results.size()-1];
  // int32_t result;
  // uint64_t cycles;
  // result_file >> result >> cycles;
  // *args->ArgOuts[0] = result;
  // *args->cycles = cycles;

}
""")
  open(s"void Application(int numThreads, cppDeliteArraystring * args) {")
  emit("Interface_t interface;")

    withStream(getStream("interface", "h")) {
      emit(s"""// Interface between delite c++ and hardware tester
  // class Interface_t {

  // public:
  //   int32_t* ArgIns[1]; // Can do sizeof this because we size arrays at codegen time
  //   int32_t* ArgOuts[1];
  //   long* MemIns[0];
  //   long* MemOuts[0][0];
  //   uint64_t* cycles;
  // };
  
#include <vector>
#include <iostream>
#include <stdint.h>

class Interface_t""")
      open("{")
      open(s"""public:""")
      emit("Interface_t()")
      emit("{}")
      emit("~Interface_t()")
      emit("{}")
    }

    withStream(getStream("cpptypes","h")) {
      emit("""#ifndef __CPPTYPES_H__
#define __CPPTYPES_H__
#include "DRAM.h" 
#endif""")
    }

    withStream(getStream("DRAM","h")){
      emit(s"""
#include <stdint.h>
#include <vector>
#include <iostream>

class DRAM {
public:
  uint64_t baseAddr;
  uint32_t size;

  DRAM(uint64_t base, int size) {
    this->baseAddr = base;
    this->size = size;
  }
  void add_mem(long num) { data.push_back(num); }
  long get_mem(int i) { return data[i]; }
  long data_length() { return data.size(); }

private:
  std::vector<long> data;

};""")
    }
    super.emitFileHeader()
  }

  override protected def emitFileFooter() {  
    close("}")
    emit(s"""
int main(int argc, char *argv[]) {
  cppDeliteArraystring *args = new cppDeliteArraystring(argc-1);
  for (int i=1; i<argc; i++) {
    args->update(i-1, *(new string(argv[i])));
  }
  int numThreads = 1;
  char *env_threads = getenv("DELITE_NUM_THREADS");
  if (env_threads != NULL) {
    numThreads = atoi(env_threads);
  } else {
    fprintf(stderr, "[WARNING]: DELITE_NUM_THREADS undefined, defaulting to 1\\n");
  }
  fprintf(stderr, "Executing with %d thread(s)\\n", numThreads);
  Application(numThreads, args);
  return 0;
}
""")

    withStream(getStream("interface", "h")) {
      emit(s"uint64_t* cycles;")
      emit(s"void add_mem(long num) { memOut.push_back(num);}")
      emit(s"long get_mem(int i) { return memOut[i]; }")
      emit(s"long memOut_length() { return memOut.size(); } // Cannot size in advance because multiple DRAMs will share this header")
      emit(s"private:")
      emit(s"std::vector<long> memOut;")
      close("")
      close("};")
    }

    super.emitFileFooter()
  }

}
