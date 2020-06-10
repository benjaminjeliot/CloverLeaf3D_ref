// sensei_bridge.h

#ifndef BRIDGE_H_
#define BRIDGE_H_

#include <conduit.hpp>

#ifdef __cplusplus
extern "C" {
#endif

int sensei_bridge_initialize();

int sensei_bridge_execute(conduit::Node* node);

int sensei_bridge_finalize();

#ifdef __cplusplus
}
#endif

#endif  // BRIDGE_H_
