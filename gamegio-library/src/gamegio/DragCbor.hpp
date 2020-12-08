#ifndef __Drag_cbor__
#define __Drag_cbor__

#include "Vec3Cbor.hpp"
#include "EntityIdCbor.hpp"

namespace cbd {

  typedef enum {
    DragEnding = 0,
    DragActive = 1,
  } EnumDragState;

  typedef struct {
    EnumDragState selector;
    struct {
      struct {
        Position value0; // in Vec3Cbor. hit position.
      } DragPosition;
    } data;
  } DragEvent;


  void readDragEvent(CborValue *it0, DragEvent *dragEvent);
  void writeDragEvent(CborEncoder *enc0, DragEvent dragEvent);

}

extern const uint64_t ctDragHitPosition;
extern const uint64_t ctDragCamera;
extern const uint64_t ctDragEvent;

#endif
