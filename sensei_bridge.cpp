// sensei_bridge.cpp

#include "sensei_bridge.h"

#include <conduit.hpp>
#include <ConduitDataAdaptor.h>
#include <ConfigurableAnalysis.h>
#include <mpi.h>

#include <vtkNew.h>
#include <vtkSmartPointer.h>
#include <vtkDataObject.h>
#include <vtkObjectBase.h>
#include <iostream>

// To fix missing ConduitDataAdaptor::PrintSelf in SENSEI
namespace sensei {
void ConduitDataAdaptor::PrintSelf(ostream &os, vtkIndent indent) {
  os << indent << "ConduitDataAdaptor::PrintSelf" << std::endl;
}
}  // namespace sensei

namespace internals
{
  static vtkSmartPointer<sensei::ConduitDataAdaptor> data_adaptor;
  static vtkSmartPointer<sensei::ConfigurableAnalysis> analysis_adaptor;
  static MPI_Comm comm;
}

int sensei_bridge_initialize() {

  std::string config_file{"sensei-config.xml"};

  //data adaptor
  internals::data_adaptor = vtkSmartPointer<sensei::ConduitDataAdaptor>::New();
  internals::data_adaptor->SetCommunicator(MPI_COMM_WORLD);

  int size{-1}, rank{-1};
  MPI_Comm_size(internals::data_adaptor->GetCommunicator(), &size);
  MPI_Comm_rank(internals::data_adaptor->GetCommunicator(), &rank);

  // analysis adaptor
  internals::analysis_adaptor = vtkSmartPointer<sensei::ConfigurableAnalysis>::New();
  if (internals::analysis_adaptor->Initialize(config_file)) {
    SENSEI_ERROR("Failed to initialize the analysis adaptor")
    return -1;
  }

  return 0;
}

int sensei_bridge_execute(conduit::Node* node) {

  // Print the JSON for the data
  // node->print_detailed();

  internals::data_adaptor->SetNode(node);
  if(!internals::analysis_adaptor->Execute(internals::data_adaptor)) {
    SENSEI_ERROR("Failed to execute analysis")
    // abort();
  }

  // internals::data_adaptor->ReleaseData();

  return 0;
}

int sensei_bridge_finalize() {
  internals::analysis_adaptor->Finalize();
  internals::analysis_adaptor = nullptr;
  internals::data_adaptor = nullptr;

  return 0;
}
