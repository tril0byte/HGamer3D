#ifndef __Hover_cbor__
#define __Hover_cbor__

#include "EntityIdCbor.hpp"
#include "Vec3Cbor.hpp"

namespace cbd {

  typedef enum {
    NoEntity = 0,
    JustEntity = 1,
  } EnumMaybeEntity;

  typedef struct {
    EnumMaybeEntity selector;
    struct {
      struct {
        EntityId value0;
        Position value1; // in Vec3Cbor. hit position.
      } HoverEntity;
    } data;
  } HoverEvent;


  void readHoverEvent(CborValue *it0, HoverEvent *hoverEvent);
  void writeHoverEvent(CborEncoder *enc0, HoverEvent hoverEvent);

}

extern const uint64_t ctHoverCamera;
extern const uint64_t ctHoverEvent;

#endif

